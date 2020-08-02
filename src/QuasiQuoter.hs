{-# LANGUAGE TemplateHaskell #-}
module QuasiQuoter where

import qualified AST
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Generics
import qualified Parser.Parser as Parser
import Parser.TokStream (Parser)
import qualified Text.Megaparsec as MP
import Text.Megaparsec (mkPos, unPos, sourceName, sourceColumn, sourceLine, SourcePos(..))
import Errors
import Metadata (SourceSpan(..), mkSourceSpan, HasSpan(..))
import Control.Monad.IO.Class
import System.Exit
import System.IO
import Parser.Configuration
import Control.Applicative.Combinators (between)

qqCfg :: ParseConfiguration
qqCfg = PConfig { qqSupport = True }

cvexpr :: QuasiQuoter
cvexpr = QuasiQuoter quoteExprExp quoteExprPat undefined undefined

cvstmt :: QuasiQuoter
cvstmt = QuasiQuoter quoteStmtExp quoteStmtPat undefined undefined

toLineCol :: MP.SourcePos -> (Int, Int)
toLineCol = (,) <$> unPos . sourceLine <*> unPos . sourceColumn

showModifiedDiagnostic :: CharPos -> CharPos -> ComponentError -> IO ()
showModifiedDiagnostic (sLine, sCol) _ err =
    let ewl@(ErrorWithLoc _ msg) = toDiagnostic err
        (SourceSpan start end _) = getSpan ewl
        filename = sourceName start
        (startLine, startCol) = toLineCol start
        (endLine, endCol) = toLineCol end
        newSpan = mkSourceSpan filename (startLine + sLine - 1, startCol + sCol - 1) (endLine + sLine - 1, endCol + sCol - 1) 0
        newDiag = errorAt newSpan msg
        flushAll = hFlush stdout *> hFlush stderr
    in between flushAll flushAll (showDiagnostics [newDiag])

posInfo :: Loc -> (String, CharPos, CharPos)
posInfo = (,,) <$> TH.loc_filename <*> TH.loc_start <*> TH.loc_end

toExpQWithAntiquoter :: Data a => a -> TH.ExpQ
toPatQWithAntiquoter :: Data a => a -> TH.PatQ
toExpQWithAntiquoter = dataToExpQ (const Nothing `extQ` antiquoterExp)
toPatQWithAntiquoter = dataToPatQ (const Nothing `extQ` antiquoterPat)

quoteStmtExp, quoteExprExp :: String -> TH.ExpQ
quoteStmtPat, quoteExprPat :: String -> TH.PatQ
quoteExprExp = quoteParser toExpQWithAntiquoter Parser.expr
quoteExprPat = quoteParser toPatQWithAntiquoter Parser.expr

quoteStmtExp = quoteParser toExpQWithAntiquoter Parser.statement
quoteStmtPat = quoteParser toPatQWithAntiquoter Parser.statement

quoteParser :: Data a => (a -> TH.Q b) -> Parser a -> String -> TH.Q b
quoteParser fn parser str = do
    th_loc <- TH.location

    let (filename, sPos@(sLine, sCol), ePos@(eLine, eCol)) = posInfo th_loc
        result = Parser.runWithConfig qqCfg (parser <* MP.eof) filename str
        fixSourcePos :: SourcePos -> SourcePos
        fixSourcePos sp = let line  = unPos (sourceLine sp)
                              col   = unPos (sourceColumn sp)
                              line' = line + sLine - 1
                              col'  = col + sCol - 1
                              in if line == 1
                                    then sp{sourceLine = mkPos line', sourceColumn = mkPos col'}
                                    else sp{sourceLine = mkPos line'}

        -- Edit spans to account for the fact that the tokenized/parsed source starts at an offset.
        fixSpan :: SourceSpan -> SourceSpan
        fixSpan sp@SourceSpan{ssStart=s, ssEnd=e} = sp{ssStart = fixSourcePos s,
                                                         ssEnd = fixSourcePos e}

    case result of
      Left err -> liftIO $ showModifiedDiagnostic sPos ePos err *> exitFailure
      Right result -> fn (everywhere (mkT fixSpan) result)

applyConE :: Name -> Name -> TH.ExpQ
applyConE c val = TH.appE (TH.conE c) (TH.varE val)

antiquoterExp :: AST.Expr -> Maybe TH.ExpQ
antiquoterExp (AST.AntiInt s span) = Just $ 'AST.IntConst `applyConE` TH.mkName s `TH.appE` liftData span
antiquoterExp (AST.AntiBool s span) = Just $ 'AST.BoolConst `applyConE` TH.mkName s `TH.appE` liftData span
antiquoterExp (AST.AntiExpr s span) = Just . TH.varE $ TH.mkName s
antiquoterExp _ = Nothing

antiquoterPat :: AST.Expr -> Maybe TH.PatQ
antiquoterPat (AST.AntiInt s span) = Just $ TH.conP 'AST.IntConst [TH.varP (TH.mkName s), TH.varP . TH.mkName $ "span_" ++ s]
antiquoterPat (AST.AntiBool s span) = Just $ TH.conP 'AST.BoolConst [TH.varP $ TH.mkName s, TH.varP . TH.mkName $ "span_" ++ s]
antiquoterPat (AST.AntiExpr s _) = Just . TH.varP $ TH.mkName s
antiquoterPat _ = Nothing
