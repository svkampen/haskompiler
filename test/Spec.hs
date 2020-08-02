{-# LANGUAGE PartialTypeSignatures #-}
import Test.Hspec
import Parser
import Errors
import Data.List (isSubsequenceOf)
import qualified AST

runCompiler :: String -> Either [Diagnostic] [AST.Decl]
runCompiler source = case runTopLevel "<input>" source of
                Left err -> Left [toDiagnostic err]
                Right res -> Right res

shouldContainError :: Either [Diagnostic] [AST.Decl] -> String -> Expectation
shouldContainError (Right _) _ = expectationFailure "no error"
shouldContainError (Left ds) substr = any (diagContainsError substr) ds `shouldBe` True
    where diagContainsError s (ErrorWithLoc _ s') = s `isSubsequenceOf` s'
          diagContainsError _ _ = False

compilationShouldErrorWith :: FilePath -> String -> Expectation
compilationShouldErrorWith fp err = do
    f <- readFile fp
    runCompiler f `shouldContainError` err

main :: IO ()
main = hspec $ do
    describe "Lexer tests" $
        it "should fail with an unexpected token" $ do
            f <- readFile "test/fail/lexerr.civic"
            runCompiler f `shouldContainError` "invalid token"

    describe "Parser tests" $
        it "should fail with an unexpected EOF" $
            "test/fail/parseerr.civic" `compilationShouldErrorWith` "unexpected end of input"
