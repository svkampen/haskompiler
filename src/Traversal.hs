{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, RankNTypes, InstanceSigs, GADTs, TemplateHaskell #-}
module Traversal
    (
      ASTTraversal(..), TraversalState(..),

      -- Extra generic traversals
      everywhereM', everywhereUntilMatchM',

      -- Error handling
      emitDiagnostic, emitDiagnostics,

      -- Symbol table modifications (lifted into ASTTraversal)
      astSTup, astSTdown, astAddSymbol, mapSTM, modifySTM, mapST, astGlobalLookup, astLocalLookup,

      -- Traversal data modification
      modifyTD,

      -- Generic map/modify over a component of the Traversal state.
      mapComponentM, modifyComponentM, mapComponent,

      symbolTable, traversalData
    )

    where

import Control.Lens hiding (children)
import Control.Monad.State
import Symbol
import Errors (Diagnostic(..))
import Data.Generics

-- | The state component of an 'ASTTraversal'.
data TraversalState custom = TraversalState
    { _symbolTable :: STZipper -- ^ The symbol table associated with the AST that is being traversed.
    , _errors :: [Diagnostic]  -- ^ Compiler diagnostics generated during this traversal.
    , _traversalData :: custom -- ^ Custom data which can vary from traversal to traversal.
    } deriving (Show)

makeLenses ''TraversalState

-- | The AST traversal monad. Simply a wrapper around `State`.
newtype ASTTraversal custom a = ASTTraversal { unASTT :: State (TraversalState custom) a } deriving (Functor, Applicative, Monad, MonadState (TraversalState custom))

instance MonadFail (ASTTraversal cC) where
    fail = error

-- | Emit a diagnostic, adding it to the diagnostic stack in an 'ASTTraversal'
emitDiagnostic :: Diagnostic -> ASTTraversal c ()
emitDiagnostic d = modifyComponentM errors $ return . (d:)

-- | Emit a list of diagnostics. See 'emitDiagnostic'.
emitDiagnostics :: [Diagnostic] -> ASTTraversal c ()
emitDiagnostics = mapM_ emitDiagnostic

-- | Monadic modification over state.
modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

-- | Monadic modification over a component of the state.
modifyComponentM :: MonadState state m => Lens' state component -> (component -> m component) -> m ()
modifyComponentM lens fun = modifyM $ lens fun

-- | Monadic mapping over a component of the state.
mapComponentM :: MonadState state m => Lens' state component -> (component -> m result) -> m result
mapComponentM l = (gets (^. l) >>=)

-- | Mapping over a component of the state. 
mapComponent :: forall state m component result. MonadState state m => Lens' state component -> (component -> result) -> m result
mapComponent l = (<$> gets (^. l))

-- | Monadic modification of the `STZipper` of an `ASTTraversal`.
modifySTM :: (STZipper -> ASTTraversal c STZipper) -> ASTTraversal c ()
modifySTM = modifyComponentM symbolTable

-- | `mapM` over the `STZipper` of an `ASTTraversal`.
mapSTM :: (STZipper -> ASTTraversal c a) -> ASTTraversal c a
mapSTM = mapComponentM symbolTable

-- | `map` over the `STZipper` of an `ASTTraversal`
mapST :: (STZipper -> a) -> ASTTraversal c a
mapST = mapComponent symbolTable

-- | Modification of the custom data in an `ASTTraversal`.
modifyTD :: (c -> c) -> ASTTraversal c ()
modifyTD f = modifyComponentM traversalData (return . f)

-- | Symbol lookup in an `ASTTraversal`.
astLocalLookup, astGlobalLookup :: (Symbol -> Bool) -> ASTTraversal c (Maybe Symbol)
astLocalLookup = mapST . localLookup
astGlobalLookup = mapST . globalLookup

-- | Symbol addition in an `ASTTraversal`.
astAddSymbol :: Symbol -> ASTTraversal c ()
astAddSymbol = modifySTM . zipperAddSymbol

-- | Movement upwards in the symbol table zipper of an `ASTTraversal`.
astSTup :: ASTTraversal c ()
astSTup = modifySTM up

-- | Movement downwards in the symbol table zipper of an `ASTTraversal`.
astSTdown :: Int -> ASTTraversal c ()
astSTdown = modifySTM . down

-- | Run a generic, monadic function everywhere while it returns @Nothing@ (indicating the absence of a type-specific case).
-- Terminate when a type-specific case (returning @Just _@) is run.
--
-- Intended to be used with type-specific functions which traverse into child structures on their own.
-- Traverses in a top-down fashion (the only way that makes sense given the description above).
everywhereUntilMatchM' :: forall m. Monad m => (forall d. Data d => d -> m (Maybe d)) -> (forall d. Data d => d -> m d)
everywhereUntilMatchM' f = go
    where go :: forall d. Data d => d -> m d
          go x = do
              x' <- f x
              case x' of
                Just y -> return y
                Nothing -> gmapM go x

-- | Version of 'everywhereM' which traverses in a top-down fashion.
everywhereM' :: forall m. Monad m => (forall d. Data d => d -> m d) -> (forall d. Data d => d -> m d)
everywhereM' f = go
    where go :: GenericM m
          go x = do
            x' <- f x
            gmapM go x'
