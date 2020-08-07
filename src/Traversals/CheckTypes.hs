{-# LANGUAGE RecordWildCards, LambdaCase, GADTs, DataKinds, QuasiQuotes, ScopedTypeVariables, TypeFamilies, TypeApplications #-}
module Traversals.CheckTypes
    (ctTraversal, runCTTraversal)
    where

import GHC.Records
import Traversal
import AST
import Errors
import Symbol
import Data.Data hiding (typeRep)
import Control.Monad.State
import Text.Printf
import Control.Lens
import Debug.Trace
import Metadata (HasSpan)
import Control.Monad.Extra

newtype TypeInfo = Info { inferredType :: Type } deriving (Show)

type CTTraversal = ASTTraversal TypeInfo
type FunctionName = String
type Params = [Expr]

inferType :: Type -> CTTraversal ()
inferType t = modifyTD (const (Info t))

getInferredType :: CTTraversal Type
getInferredType = mapComponent traversalData (\(Info t) -> t)

smartQuote :: String -> String
smartQuote = printf "‘%s’"

ctBinop :: BinopType -> Expr -> Expr -> Expr -> CTTraversal Expr
ctBinop binopType totalExpr l r = do
    tl <- travDown l >> getInferredType
    tr <- travDown r >> getInferredType

    when (tl /= tr) $ emitDiagnostics [
        errorAt totalExpr (printf "Operands to '%s' have differing types" (show binopType)),
        noteAt l (printf "Left operand has type %T" tl),
        noteAt r (printf "Right operand has type %T" tr)]

    inferType tl

    handleEdgeCases
    return totalExpr
    where handleEdgeCases | binopType `elem` [Add, Mul] = pure ()
                          | binopType `elem` [Sub, Div, Mod, LessThan, LessEqual, GreaterEqual, GreaterThan] = rejectBool' <* inferType TBool
                          | binopType `elem` [Equal, NotEqual] = inferType TBool
                          | binopType `elem` [And, Or] = requireBool'

          rejectBool'  = rejectBool totalExpr
          requireBool' = requireBool totalExpr

rejectBool :: HasSpan a => a -> CTTraversal ()
rejectBool totalExpr = do
  ty <- getInferredType
  when (ty == TBool) $ totalExpr ❌ "operator does not support boolean operands"

requireBool :: HasSpan a => a -> CTTraversal ()
requireBool totalExpr = do
  ty <- getInferredType
  when (ty /= TBool) $
    totalExpr ❌ printf "operator only supports boolean operands"

-- | Check whether parameters to a function call correspond to the parameters given in the function definition.
checkCallParameters :: SymbolTable -> Params -> CTTraversal ()
checkCallParameters (Node _ children) exprs = mapM_ checkParam (zip3 [(1::Int)..] children exprs)
    where checkParam (i, LeafNode var, exp) = do
            expType <- travDown exp >> getInferredType
            let varType = (getField @"varType" var)
            when (expType /= varType) $ do
                exp ❌ printf "function parameter %d: invalid type, expected %T but expression has type %T" i varType expType


validateCall :: HasSpan a => a -> FunctionName -> Params -> CTTraversal ()
validateCall loc name params = do
    info <- astGlobalLookup (fnMatch name)
    case info of
        Nothing -> error "name not found in CT traversal"
        Just fnNode -> do
            inferType . returnType . value $ fnNode
            let given    = length params
                expected = numParams (value fnNode)
            if (given /= expected)
                then loc ❌ printf "invalid number of function parameters; %d given, %d expected" given expected
                else checkCallParameters fnNode params
    return ()

ctExpr :: Expr -> CTTraversal Expr
-- Literals
ctExpr e@IntConst{} = inferType TInt >> return e
ctExpr e@BoolConst{} = inferType TBool >> return e
ctExpr e@FloatConst{} = inferType TFloat >> return e

ctExpr e@(CallExpr name params _) = do
    validateCall e name params
    return e

ctExpr e@(Var name _) = do
    info <- astGlobalLookup (varMatch name)
    maybe (error "name not found in CT traversal?") (inferType . Symbol.varType . value) info
    return e

ctExpr e@(Binop ty l r _) = ctBinop ty e l r
ctExpr e@(Cast to _ _) = inferType to >> return e
ctExpr e@(Neg e' _) = ctExpr e' <* rejectBool e
ctExpr e@(Not e' _) = ctExpr e' <* requireBool e <* inferType TBool
-- ^ Inferring TBool at the end here is usually redundant, but it improves
-- diagnostics by generating a single failure diagnostic instead of multiple.

ctFunction :: Function -> CTTraversal Function
ctFunction fn@AST.Function{..} = do
    astSTdownToMatch (fnMatch fnName)
    gmapM travDown fn
    astSTup
    return fn

ctStmt :: Statement -> CTTraversal Statement
ctStmt stmt@(CallStmt name params _) = validateCall stmt name params >> return stmt

ctStmt stmt@(If e tblock fblock _) = do
    conditionTy <- travDown e >> getInferredType
    when (conditionTy /= TBool) $ emitDiagnostics [
        errorAt e "condition is not a boolean expression.",
        noteAt e $ printf "the given expression is of type %T." conditionTy]
    travDown tblock
    travDown fblock
    return stmt

ctStmt stmt = return stmt

ctVardecl :: VarDecl -> CTTraversal VarDecl
ctVardecl v@VarDecl{..} = do
    whenJust initializer $ \init -> do
        itype <- travDown init >> getInferredType
        when (itype /= vtype) $ do
            init ❌ printf "variable of type %T cannot be initialized with an expression of type %T." vtype itype

    return v

travDown :: Data d => d -> CTTraversal d
travDown = ctTraversal

ctTraversal :: forall d. Data d => d -> CTTraversal d
ctTraversal = everywhereUntilMatchM' funStack
    where funStack :: forall d. Data d => d -> CTTraversal (Maybe d)
          funStack = mkM' ctExpr `extM'` ctStmt `extM'` ctVardecl `extM'` ctFunction

runCTTraversal :: Data d => (d, TraversalState c) -> (d, TraversalState TypeInfo)
runCTTraversal (res, state) = runState (unASTT $ ctTraversal res) $ (traversalData %~ const (Info AST.TVoid)) state

(<$$>) :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
(<$$>)= (fmap . fmap)

mkM' :: forall m a b. (Monad m, Typeable a, Typeable b) => (a -> m a) -> b -> m (Maybe b)
mkM' f =
    case eqT @a @b of
      Just Refl -> Just <$$> f
      _ -> return . const Nothing

type WrappedMaybe m a = (a -> m (Maybe a)) 

extM' :: forall m a b. (Monad m, Typeable a, Typeable b) => WrappedMaybe m a -> (b -> m b) -> WrappedMaybe m a
extM' def ext =
    case eqT @a @b of
        Just Refl -> Just <$$> ext
        _ -> def