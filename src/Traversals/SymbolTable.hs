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
import Control.Monad.Extra
import Text.Printf

type STTraversal = ASTTraversal ()

-- | Run an action, unless a symbol definition redefines a symbol of the same type in the local scope.
unlessRedefinition :: Symbol -> STTraversal () -> STTraversal ()
unlessRedefinition sym | Function{..} <- sym = unlessRedefinition' (fnMatch name)
                       | Variable{..} <- sym = unlessRedefinition' (varMatch name)
    where unlessRedefinition' matcher action = do
            match <- (value <$>) <$> astLocalLookup matcher
            case match of
                Just sym' -> emitDiagnostics [
                    errorAt sym $ printf "redefinition of symbol %s" (smartQuote $ name sym),
                    noteAt sym' "previously defined here"]
                Nothing -> action

smartQuote :: String -> String
smartQuote = printf "‘%s’"

stFunction :: AST.Function -> STTraversal AST.Function
stFunction fn@AST.Function{..} = do
    let sym = fun fnReturnType fnName (length fnParams) span

    unlessRedefinition sym $ do
        astAddSymbol (fun fnReturnType fnName (length fnParams) span)
        astSTdown 0
        mapM_ addParams fnParams
        astSTup

    return fn
    where addParams (AST.Param ty name span) = do
            let sym = var ty name span
            unlessRedefinition sym $
                astAddSymbol (var ty name span)


stLocalVars :: AST.Funbody -> STTraversal AST.Funbody
stLocalVars fb@(AST.FB vardecls _) = do
    astSTdown 0
    mapM_ addVardecl vardecls
    astSTup
    return fb
        where addVardecl vd@(AST.VarDecl ty name _ span) = do
                let sym = var ty name span
                unlessRedefinition sym $ astAddSymbol sym

stGlobals :: AST.Global -> STTraversal AST.Global
stGlobals glob@AST.Global{..} = do
    let sym = var varType name __span
    unlessRedefinition sym $ astAddSymbol sym
    return glob

stTraversal :: Data d => d -> STTraversal d
stTraversal = everywhereM' (mkM stFunction `extM` stLocalVars `extM` stGlobals)

runSTTraversal :: Data d => (d, TraversalState c) -> (d, TraversalState ())
runSTTraversal (res, state) = runASTTraversal (stTraversal res) (state & traversalData %~ const ())
