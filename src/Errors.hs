{-# LANGUAGE TypeFamilies, RecordWildCards, RankNTypes, FlexibleInstances, ScopedTypeVariables, ExistentialQuantification #-}
module Errors
    (Diagnostic(..), IsDiagnostic(..), errorAt, noteAt, warningAt, ComponentError(..), showDiagnostics, isError, containsErrors)
    where

import Metadata (HasSpan(..), WithPos(..), SourceSpan(..), mkSourceSpan)
import GHC.Stack
import Text.Printf
import Parser.TokStream (TokStream)
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Text.Regex.PCRE.Light.Char8 as Regex
import Data.List.NonEmpty (NonEmpty(..))
import Text.Megaparsec (SourcePos(..), ParseError(..), ErrorItem(..), ParseErrorBundle(..), attachSourcePos, errorOffset, unPos)
import qualified Data.Set as Set
import Control.Monad
import System.Console.ANSI
import Data.List (intersperse)

-- | An error occurring in a component of the compilation pipeline (tokenizer, parser, ...)
data ComponentError
    = TokenizerError FilePath String
    | ParserError (ParseErrorBundle TokStream Void)
    deriving (Show)

-- | Generate a `Diagnostic` from a tokenizer error.
genTokenizerDiagnostic :: FilePath -> String -> Diagnostic
genTokenizerDiagnostic fname err =
    let r = Regex.compile "lexical error at line (\\d+), column (\\d+)" []
        Just [_, lineStr, colStr] = Regex.match r err []
        (lineNo, colNo) = (read lineStr, read colStr)
        sspan = mkSourceSpan fname (lineNo, colNo) (lineNo, colNo) 0
     in errorAt sspan "invalid token"

-- | Generate a `Diagnostic`  from a parser error.
genParserDiagnostic :: HasCallStack => ParseErrorBundle TokStream Void -> Diagnostic
genParserDiagnostic ParseErrorBundle{..} = NE.head diagnostics
    where
      (errorsWithSP, _) = attachSourcePos errorOffset bundleErrors bundlePosState

      showItem (Tokens (WithPos{..}:|[])) = show value
      showItem EndOfInput = "end of input"
      showItem (Label s) = NE.toList s
      showItem _ = error "Unable to show item"

      toDiagnostic (TrivialError _ unexpected expected, sp) =
          let unexpectedStr = case unexpected of
                                Just item -> showItem item
                                Nothing -> error "parser error with no unexpected item?"
              expectedStr = foldr1 (\a b -> a ++ ", " ++ b) (Set.map showItem expected)
              msg = printf "unexpected %s, expected one of {%s}" unexpectedStr expectedStr
              endSP = case unexpected of
                        Just (Tokens (twp:|[])) -> endPos twp
                        _ -> sp
           in errorAt (SourceSpan sp endSP 0) msg
      toDiagnostic (FancyError _ _, _) = error "can't handle fancy errors"

      diagnostics = toDiagnostic <$> errorsWithSP

-- | Data types which can be converted into `Diagnostic`s
class IsDiagnostic a where
    toDiagnostic :: a -> Diagnostic

instance IsDiagnostic ComponentError where
    toDiagnostic (TokenizerError fp s) = genTokenizerDiagnostic fp s
    toDiagnostic (ParserError peb) = genParserDiagnostic peb

-- | Compiler diagnostics (errors, warnings, and notes).
--
-- Uses existential quantification to store a value which is an instance of `HasSpan`,
-- which is taken to be the location of the diagnostic, along with a message describing the diagnostic.
data Diagnostic = forall a. HasSpan a => ErrorWithLoc { loc :: a, err :: String }   -- ^ A `Diagnostic` representing an error.
                | forall a. HasSpan a => NoteWithLoc { loc :: a, err :: String }    -- ^ A `Diagnostic` representing a note.
                | forall a. HasSpan a => WarningWithLoc { loc :: a, err :: String } -- ^ A `Diagnostic` representing a warning.

-- | \'Lift\' a function on `HasSpan` instances to a function on `Diagnostic`s
liftD :: forall b. (forall a. HasSpan a => a -> b) -> Diagnostic -> b
liftD f (ErrorWithLoc l _) = f l
liftD f (NoteWithLoc l _ ) = f l
liftD f (WarningWithLoc l _) = f l

-- | `errorAt loc msg` constructs an error at a location.
errorAt :: HasSpan a => a -> String -> Diagnostic
errorAt = ErrorWithLoc

-- | `noteAt loc msg` constructs a note at a location.
noteAt :: HasSpan a => a -> String -> Diagnostic
noteAt = NoteWithLoc

-- | `warningAt loc msg` constructs a warning at a location.
warningAt :: HasSpan a => a -> String -> Diagnostic
warningAt = WarningWithLoc

instance HasSpan Diagnostic where
    getSpan = liftD getSpan

instance MonadFail (Either Diagnostic) where
    fail = Left . ErrorWithLoc ()

instance Show Diagnostic where
    show ewl = printf "Error at %s: %s" (show $ liftD getSpan ewl) (err ewl)

-- | `splitBetween n n' lst` splits a list into a 3-tuple of (before `n`, between n and `n', at and after `n')
splitBetween :: Int -> Int -> [b] -> ([b], [b], [b])
splitBetween i i' str = (pre, mid, post)
    where (pre, mid') = splitAt i str
          (mid, post) = splitAt (i' - i) mid'

-- | Left-pad a `String` to a given length.
lpad :: Int -> String -> String
lpad m xs = replicate (m - length ys) ' ' ++ ys
    where ys = take m xs

-- | Check whether a list of `Diagnostic`s contains an error.
containsErrors :: [Diagnostic] -> Bool
containsErrors = any isError

-- | Predicate which returns `True` if the given `Diagnostic` is an error.
isError :: Diagnostic -> Bool
isError ErrorWithLoc{} = True
isError _ = False

-- | Print a list of `Diagnostic`s.
showDiagnostics :: [Diagnostic] -> IO ()
showDiagnostics = sequence_ . intersperse newline . map showDiagnostic . reverse
    where showDiagnostic (ErrorWithLoc loc msg) = showDiagnostic' (getSpan loc) msg "error" Red
          showDiagnostic (NoteWithLoc loc msg) = showDiagnostic' (getSpan loc) msg "note" Cyan
          showDiagnostic (WarningWithLoc loc msg) = showDiagnostic' (getSpan loc) msg "warning" Magenta

          setBold = setSGR [SetConsoleIntensity BoldIntensity]
          setColor c = setSGR [SetColor Foreground Vivid c]
          reset = setSGR []
          newline = putChar '\n'

          toLineCol :: SourcePos -> (Int, Int)
          toLineCol = (,) <$> unPos . sourceLine <*> unPos . sourceColumn

          showDiagnostic' ss@SourceSpan{..} msg diagType color = do
            let (startLine, startCol) = toLineCol ssStart
                (endLine, endCol)     = toLineCol ssEnd
                fp                    = sourceName ssStart

            text <- lines <$> readFile fp

            let (_, mid, _) = splitBetween (startLine - 1) endLine text 
                lineNoLength = maximum $ map (length . show) [startLine, endLine]
                gutter = replicate (lineNoLength + 2) ' ' ++ "│ "
                lnGutter i = ' ':lpad lineNoLength (show i) ++ " │ "
                putUnderline useCaret start len = do
                    let uL = if useCaret then '^':replicate (len - 1) '~' else replicate len '~'
                    putStr gutter >> setColor color >> putStr (replicate start ' ') >> putStr uL >> reset >> newline
                putMiddleLine (i, line) = do
                    let (whitespace, restOfLine) = span (==' ') line
                    putStr (lnGutter i) >> setColor color >> putStrLn line >> reset
                    putUnderline False (length whitespace) (length restOfLine)

            setBold >> printf "%s: " (show ss) >> setColor color >> printf "%s: " diagType >> reset >> putStrLn msg

            putStrLn gutter

            let ([first], middle, last) = splitBetween 1 (length mid - 1) mid
            if null last then do
              let (prefix, err, suffix) = splitBetween (startCol - 1) endCol first
              putStr (lnGutter startLine) >> putStr prefix >> setColor color >> putStr err >> reset >> putStrLn suffix
              putUnderline True (startCol - 1) (length err)
            else do
              let (prefix, err) = splitAt (startCol - 1) first
              putStr (lnGutter startLine) >> putStr prefix >> setColor color >> putStrLn err >> reset
              putUnderline True (startCol - 1) (length err)

            mapM_ putMiddleLine (zip [startLine + 1..] middle)

            unless (null last) $ do
                let (err, suffix) = splitAt endCol (head last)
                putStr (lnGutter endLine) >> setColor color >> putStr err >> reset >> putStrLn suffix
                putUnderline False 0 (length err)
