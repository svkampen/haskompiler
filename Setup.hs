import Distribution.Simple
import System.Process
import Control.Monad
import System.Directory

main :: IO ()
main = defaultMainWithHooks userHooks

userHooks :: UserHooks
userHooks = simpleUserHooks { postConf = generateLexerHook }

generateLexerHook _ _ _ _ = do
    mtimeGenerated <- getModificationTime "src/Tokens.hs"
    mtimeSource <- getModificationTime "lexer/Tokens.x"
    when (mtimeGenerated < mtimeSource) $ do
      putStrLn "Generating lexer..."
      createProcess $ shell "alex lexer/Tokens.x -o src/Tokens.hs"
      return ()

