{-# LANGUAGE PartialTypeSignatures, ExplicitForAll, FlexibleContexts, TypeFamilies, RecordWildCards #-}
module Parser.TokStream (Parser, TokStream, mkStream, nextTokens, previousTokens) where 

import Tokens
import Data.Proxy
import qualified Data.List as DL
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as MP
import Safe (headMay)
import Data.Void
import Parser.Configuration
import Control.Monad.Reader

type Parser = MP.ParsecT Void TokStream (Reader ParseConfiguration)

-- | A stream of Civic tokens.
data TokStream = TokStream
    {
        streamOffset :: !Int,                   -- ^ The current offset in the input.
        streamInput :: String,                  -- ^ The remaining stream input.
        nextTokens :: [WithPos CivicToken],     -- ^ The remaining tokens in the stream.
        previousTokens :: [WithPos CivicToken]  -- ^ The processed tokens in the stream.
    } deriving (Show)

--tokensLength :: Proxy t -> NonEmpty (WithPos CivicToken) -> Int
--tokensLength Proxy xs = let starts = startPos <$> xs
--                            groups = NE.groupBy ((==) `on` MP.sourceLine) . NE.sort $ starts
--                         in sum $ tokSpan <$> groups
--                        where tokSpan xs = ((-) `on` MP.unPos . MP.sourceColumn) (maximum xs) (minimum xs)

-- | Seek the `TokStream` forward to a given token.
seekTo :: WithPos CivicToken -> TokStream -> TokStream
seekTo twp TokStream{..} = let newOffset = startOffset twp 
                               delta = newOffset - streamOffset
                               postStr = drop delta streamInput
                               (pre, post) = break (== twp) nextTokens
                            in TokStream newOffset postStr post (previousTokens ++ pre)

-- | Seek to the end of the given `TokStream`.
seekToEnd :: TokStream -> TokStream
seekToEnd TokStream{..} = TokStream (streamOffset + length streamInput) "" [] (previousTokens ++ nextTokens)

instance MP.Stream TokStream where
    type Token TokStream = WithPos CivicToken
    type Tokens TokStream = [WithPos CivicToken]

    tokenToChunk Proxy = pure
    tokensToChunk Proxy = id

    chunkToTokens Proxy = id
    chunkLength Proxy = length

    -- is this chunk empty?
    chunkEmpty Proxy = null

    take1_ TokStream{nextTokens=[]} = Nothing
    take1_ stream@TokStream{nextTokens=[t]} = Just (t, seekToEnd stream)
    take1_ stream@TokStream{nextTokens=t:ts} = Just (t, seekTo (head ts) stream)

    takeN_ n stream@TokStream{nextTokens=s}
      | n <= 0 = Just (mempty, stream)
      | null s = Nothing
      | otherwise = let (x, s') = splitAt n s
                     in case NE.nonEmpty x of
                          Nothing -> Just (x, stream)
                          Just _  -> Just (x, seekTo (head s') stream)

    takeWhile_ f stream@TokStream{nextTokens=s} =
        let (x, s') = DL.span f s
         in case NE.nonEmpty x of
              Nothing -> (x, stream)
              Just _  -> (x, seekTo (head s') stream)

    showTokens Proxy = unwords . NE.toList . fmap (show . value)

    reachOffset o MP.PosState {..} =
        (prefix ++ restOfLine,
         MP.PosState { pstateInput = seekTo (head post) pstateInput,
                    pstateOffset = max pstateOffset o,
                    pstateSourcePos = newSourcePos,
                    pstateTabWidth = pstateTabWidth,
                    pstateLinePrefix = prefix
                  }
        )
        where
            (pre, post) = splitAt (o - pstateOffset) (nextTokens pstateInput)
            newSourcePos = case post of
                             [] -> endPos (last pre)
                             (x:_) -> startPos x
            (preStr, postStr) = case headMay post of
                                  Just head -> lineSplitAt (startPos head) pstateInput
                                  Nothing -> (last . lines $ streamInput pstateInput, "")
            restOfLine = takeWhile (/= '\n') postStr
            sameLine = MP.sourceLine newSourcePos == MP.sourceLine pstateSourcePos
            prefix = if sameLine then pstateLinePrefix else last . lines $ preStr

lineSplitAt :: MP.SourcePos -> TokStream -> (String, String)
lineSplitAt MP.SourcePos {..} TokStream {..} = let line = lines streamInput !! (MP.unPos sourceLine - 1)
                                                in splitAt (MP.unPos sourceColumn) line

-- Make a `TokStream` from a piece of Civic code, tokenizing it first.
mkStream :: FilePath -> String -> Either String TokStream
mkStream fpath s = do
    tokens <- tokenize fpath s
    return $ TokStream 1 s tokens []

-- | Generate tokens for a piece of Civic code.
tokenize :: FilePath -> String -> Either String [WithPos CivicToken]
tokenize fpath input = result
    where result = runAlex input $ do
            setFilename fpath
            let loop = do tok@WithPos{..} <- alexMonadScan
                          if value == EOF
                              then return []
                              else (tok:) <$> loop
            loop

