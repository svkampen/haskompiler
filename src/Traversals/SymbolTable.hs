{-# LANGUAGE RecordWildCards #-}
module Traversals.SymbolTable
    (stTraversal, runSTTraversal)
    where

import Traversal
import qualified AST
import Errors
import Symbol
import Data.Generics
import Control.Monad
import Control.Lens
import Text.Printf

type STTraversal = ASTTraversal ()

smartQuote :: String -> String
smartQuote = printf "‘%s’"

fnMatch :: String -> Symbol -> Bool
fnMatch matchName Function{..} = name == matchName
fnMatch _ _ = False

fnRegistrar :: AST.Function -> STTraversal AST.Function
fnRegistrar fn@AST.Function{..} = do
    match <- (value <$>) <$> astLocalLookup (fnMatch fnName)
    case match of
      Just fn' -> emitDiagnostics [errorAt fn $ printf "wait a minute, hol on...", noteAt fn' "she done already done had herses"]
      Nothing -> do
          astAddSymbol (fun fnReturnType fnName span)
          maybe (return ()) (void . varRegistrar) fnBody
    return fn


varRegistrar :: AST.Funbody -> STTraversal AST.Funbody
varRegistrar fb@(AST.FB vardecls _) = do
    astSTdown 0
    mapM_ addVardecl vardecls
    astSTup
    return fb
        where addVardecl (AST.VarDecl ty name _ span) = do
                astAddSymbol (var ty name span)

stTraversal :: Data d => d -> STTraversal d
stTraversal = everywhereM' (mkM fnRegistrar)

runSTTraversal :: Data d => (d, TraversalState c) -> (d, TraversalState ())
runSTTraversal (res, state) = runASTTraversal (stTraversal res) (state & traversalData %~ const ())
