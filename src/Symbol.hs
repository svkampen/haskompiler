{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, InstanceSigs, ScopedTypeVariables, RankNTypes, RecordWildCards, GADTs, DuplicateRecordFields, DeriveDataTypeable, TemplateHaskell #-}

module Symbol
    (
      -- Structures
      Symbol(..), SymbolTable(..), Ctx(..), Loc(..), STZipper,

      -- Symbol creation functions
      var, fun,

      -- Lenses
      struct,

      topLoc, symTable,
      down, up,
      localLookup, globalLookup, zipperAddSymbol
    )
    where

import qualified AST
import Text.Printf
import Control.Lens hiding (children)
import Data.Typeable
import Data.Data
import Safe (headMay)
import Metadata (SourceSpan(..), HasSpan(..))

data Symbol
  = Variable
      { name :: String
      , varType :: AST.Type
      , visibility :: AST.Visibility
      , frameOffset :: !Int
      , globalNum :: !Int
      , importExportNum :: !Int
      , definition :: SourceSpan
      }
  | Function
      { name :: String
      , returnType :: AST.Type
      , visibility :: AST.Visibility
      , numParams :: Int
      , functionNumber :: Int
      , definition :: SourceSpan
      }
  | Program
  deriving (Eq, Data, Typeable)

instance HasSpan Symbol where
    getSpan = definition

instance Show Symbol where
    show Variable{..} = printf "%s %s" (show varType) name
    show Function{..} = printf "%s %s()" (show returnType) name
    show Program = "<top-level program>"

data SymbolTable = Node { value :: Symbol, _children :: [SymbolTable] } | LeafNode { value :: Symbol }
    deriving (Eq)

children :: SymbolTable -> [SymbolTable]
children (LeafNode _) = []
children (Node _ c) = c

instance Show SymbolTable where
    show (Node val children) = printf "Node '%s' (children: %s)" (show val) (show children)
    show (LeafNode val)      = printf "Leaf '%s'" (show val)

data Ctx where
    Top :: Ctx
    Child :: Ctx -> [SymbolTable] -> Symbol -> [SymbolTable] -> Ctx
    deriving (Show, Eq)

data Loc c a = Loc { _ctx :: c, _struct :: a} deriving (Show, Eq)

makeLenses ''Loc

type STZipper = Loc Ctx SymbolTable

topLoc :: SymbolTable -> STZipper
topLoc = Loc Top

fun, var :: AST.Type -> String -> SourceSpan -> Symbol
fun ty name = Function name ty AST.Internal 0 0
var ty name = Variable name ty AST.Internal 0 0 0

symTable :: SymbolTable
symTable = Node Program []

addSymbol :: MonadFail m => Symbol -> SymbolTable -> m SymbolTable
addSymbol sym (Node val children) = return $ case sym of
                                      Variable{..} -> Node val (LeafNode sym:children)
                                      _ -> Node val (Node sym []:children)

addSymbol _ (LeafNode _) = fail "Can't add a symbol to a leaf node"

zipperAddSymbol :: MonadFail m => Symbol -> STZipper -> m STZipper
zipperAddSymbol = struct . addSymbol

-- Symbol table traversal up / down
up :: MonadFail m => STZipper -> m STZipper
up (Loc (Child ctx' lefts val rights) node) = return $ Loc ctx' (Node val (lefts ++ node:rights))
up (Loc Top _) = fail "Can't go up past the root node"

down :: MonadFail m => Int -> STZipper -> m STZipper
down _ (Loc _ (LeafNode _)) = fail "Can't go down a leaf node"
down idx (Loc ctx (Node val children)) = let (left, node':right) = splitAt idx children
                                          in return $ Loc (Child ctx left val right) node'

-- Symbol table lookups
globalFindMatches :: (Symbol -> Bool) -> STZipper -> [Symbol]
globalFindMatches matchFn (Loc ctx node) = [ x | x <- value <$> children node, matchFn x ] ++ findMatches' ctx
    where findMatches' (Child ctx' lefts sym rights) = [ x | x <- map value lefts ++ sym:map value rights, matchFn x ] ++ findMatches' ctx'
          findMatches' Top = []

localFindMatches :: (Symbol -> Bool) -> STZipper -> [Symbol]
localFindMatches matchFn (Loc ctx node) = [ x | x <- value <$> children node, matchFn x ] ++ findMatches' ctx
    where findMatches' (Child _ lefts _ rights) = [ x | x <- map value (lefts ++ rights), matchFn x ]
          findMatches' Top = []

globalLookup :: (Symbol -> Bool) -> STZipper -> Maybe Symbol
globalLookup = (headMay.) . globalFindMatches

localLookup :: (Symbol -> Bool) -> STZipper -> Maybe Symbol
localLookup = (headMay.) . localFindMatches
