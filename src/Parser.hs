module Parser
    (
        module Parser.Parser,
        module Parser.TokStream,
        module Parser.Configuration,
        runTopLevel
    )
    where

import Parser.Parser
import Parser.TokStream
import Parser.Configuration
import Text.Megaparsec (eof)
import AST
import Errors

runTopLevel :: InputName -> String -> Either ComponentError [Decl]
runTopLevel = run (decls <* eof)
