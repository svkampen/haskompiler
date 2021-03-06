{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, RecordWildCards, RankNTypes, ScopedTypeVariables #-}
module Main where

import qualified AST
import Data.Generics hiding (typeRep)
import Errors
import Metadata (getSpan)
import Parser.Configuration
import Parser.Parser (decls, runWithConfig)
import QuasiQuoter (cvexpr)
import Symbol (symTable, topLoc)
import System.Environment (getArgs)
import System.Exit
import Text.Megaparsec
import Traversal
import Traversals.CheckTypes
import Traversals.SymbolTable

parseArgs :: [String] -> IO [String]
parseArgs [] = die "Usage: haskompiler <filename.civic>"
parseArgs (fname:_) = return [fname]

sample :: String
sample = "int main() { bool a; bool b; } void foo() { int c; }"

sampleAST :: [AST.Decl]
sampleAST = let (Right res) = runWithConfig (PConfig True) (decls <* eof) "sample" sample
             in res

sampleExpr :: Int -> AST.Expr
sampleExpr c = [cvexpr|a + b * $int:c|]

-- | Constant folding for expressions.
constFold :: AST.Expr -> AST.Expr
constFold [cvexpr|$int:a + $int:b|] = AST.IntConst (a + b) (span_a <> span_b)
constFold [cvexpr|$int:a * $int:b|] = AST.IntConst (a * b) (span_a <> span_b)
constFold [cvexpr|$int:a - $int:b|] = AST.IntConst (a - b) (span_a <> span_b)
constFold [cvexpr|$int:a / $int:b|] = AST.IntConst (a `div` b) (span_a <> span_b)
constFold e@[cvexpr|-$int:a|] = AST.IntConst (-a) (getSpan e)
constFold exp = exp

cF :: Data a => a -> a
cF = everywhere (mkT constFold)
main :: IO ()
main = do
  [fname] <- parseArgs =<< getArgs
  text <- readFile fname

  let result = do
        tree <- runWithConfig (PConfig True) (decls <* eof) fname text
        return . runCTTraversal . runSTTraversal $ (cF tree, TraversalState (topLoc symTable) [] ())

  case result of
    Left err -> showDiagnostics . pure . toDiagnostic $ err
    Right res -> printResultTuple res

-- | Print the result of an `ASTTraversal`.
printResultTuple :: Show a => (a, TraversalState custom) -> IO ()
printResultTuple (res, TraversalState{..}) =
  case _errors of
      [] -> print res
      es -> if containsErrors es
                then showDiagnostics es
                else showDiagnostics es >> print res
