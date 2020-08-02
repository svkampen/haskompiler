{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TypeFamilies, TypeApplications #-}
module Traversals.CheckTypes
    (ctTraversal, ctTest, res)
    where

import QuasiQuoter
import Traversal
import AST
import Errors
import Symbol
import Data.Data hiding (typeRep)
import Data.Generics hiding (typeRep)
import Type.Reflection
import Control.Monad.State
import Text.Printf

newtype TypeInfo = Info { inferredType :: Type } deriving (Show)

type CTTraversal = ASTTraversal TypeInfo

inferType :: Type -> CTTraversal ()
inferType t = modifyTD (const (Info t))

getInferredType :: CTTraversal Type
getInferredType = mapComponent traversalData (\(Info t) -> t)

varMatch :: String -> (Symbol -> Bool)
varMatch s v@Variable{} = Symbol.name v == s
varMatch s _ = False

boRefuseBool :: Expr -> CTTraversal ()
boRefuseBool e = do
    ty <- getInferredType
    when (ty == TBool) $
       emitDiagnostic (errorAt e "binary operator does not support boolean operands")

ctBinop :: String -> Expr -> Expr -> Expr -> CTTraversal ()
ctBinop binopName totalExpr l r = do
    tl <- getInferredType << travDown l
    tr <- getInferredType << travDown r
    when (tl /= tr) $ emitDiagnostics [
        errorAt totalExpr (printf "Operands to '%s' have differing types" binopName),
        noteAt l (printf "Left operand has type %s" $ show tl),
        noteAt r (printf "Right operand has type %s" $ show tr)]
    inferType tl

ctExpr :: Expr -> CTTraversal Expr
ctExpr e@IntConst{} = inferType TInt >> return e
ctExpr e@BoolConst{} = inferType TBool >> return e
ctExpr e@FloatConst{} = inferType TFloat >> return e
ctExpr e@(Var name _) = do
    info <- astGlobalLookup (varMatch name)
    maybe (error "name not found in CT traversal?") (inferType . Symbol.varType) info
    return e

ctExpr e@(Mul e1 e2 _) = do
    ctBinop "*" e e1 e2
    return e

ctExpr e@(Mod e1 e2 _) = do
    ctBinop "%" e e1 e2
    boRefuseBool e
    return e

-- | Flipped version of '(>>)'
(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)

travDown :: Data d => d -> CTTraversal d
travDown = ctTraversal

a = (Just<$>) . ctExpr

mkM' :: forall m a b. (Monad m, Typeable a, Typeable b) => (a -> m (Maybe a)) -> b -> m (Maybe b)
mkM' f =
    case eqTypeRep (typeRep @b) (typeRep @a) of
      Just HRefl -> f
      _ -> return . const Nothing

ctTraversal :: Data d => d -> CTTraversal d
ctTraversal = everywhereUntilMatchM' (mkM' a)

ctTest :: Data d => d -> (d, TraversalState TypeInfo)
ctTest dat = runState (unASTT $ travDown dat) (TraversalState (topLoc symTable) [] (Info TVoid))

res = ctTest [cvstmt|foo = false % true;|]

