{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, InstanceSigs, ScopedTypeVariables, RankNTypes, RecordWildCards, GADTs, DuplicateRecordFields, DeriveDataTypeable, TemplateHaskell #-}

module Symbol
    (
      -- Structures
      Symbol(..), SymbolTable(..), Ctx(..), Loc(..), STZipper,

      -- Symbol creation functions
      var, fun,

      -- Match functions
      varMatch, fnMatch,

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

varMatch :: String -> (Symbol -> Bool)
varMatch s v@Variable{} = Symbol.name v == s
varMatch _ _ = False

fnMatch :: String -> (Symbol -> Bool)
fnMatch s f@Symbol.Function{} = Symbol.name f == s
fnMatch _ _ = False

children :: SymbolTable -> [SymbolTable]
children (LeafNode _) = []
children (Node _ c) = c

instance Show SymbolTable where
    show (Node val children) = printf "Node '%s' (children: %s)" (show val) (show children)
    show (LeafNode val)      = printf "Leaf '%s'" (show val)

-- | Context in an STZipper.
data Ctx where
    -- | The current node is at the top of the symbol table.
    Top :: Ctx

    -- | The current node is a child of @Node sym (lefts ++ current:rights)@
    Child :: Ctx -> Symbol -> [SymbolTable] -> [SymbolTable] -> Ctx
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
up (Loc (Child ctx' val lefts rights) node) = return $ Loc ctx' (Node val (lefts ++ node:rights))
up (Loc Top _) = fail "Can't go up past the root node"

down :: MonadFail m => Int -> STZipper -> m STZipper
down _ (Loc _ (LeafNode _)) = fail "Can't go down a leaf node"
down idx (Loc ctx (Node val children)) = let (left, node':right) = splitAt idx children
                                          in return $ Loc (Child ctx val left right) node'

-- | Transform a context along with a current node back into a symbol table node.
ctxToNode :: Ctx -> SymbolTable -> SymbolTable
ctxToNode (Child _ parentSym lefts rights) currentNode = Node parentSym (lefts ++ currentNode:rights)
ctxToNode Top node = node

-- Symbol table lookups
globalFindMatches :: (Symbol -> Bool) -> STZipper -> [SymbolTable]
globalFindMatches matchFn (Loc ctx node) = [ x | x <- children node, matchFn (value x)] ++ findMatches' ctx node
    where
        findMatches' Top _ = []
        findMatches' ctx@(Child ctx' _ lefts rights) node =
            let parentNode = ctxToNode ctx node
             in [ x | x <- (node:lefts ++ parentNode:rights), matchFn (value x) ] ++ findMatches' ctx' parentNode

localFindMatches :: (Symbol -> Bool) -> STZipper -> [SymbolTable]
localFindMatches matchFn (Loc ctx node) = [ x | x <- children node, matchFn (value x) ] ++ findMatches' ctx node
    where
        findMatches' Top _ = []
        findMatches' (Child _ _ lefts rights) node =
            let -- parentNode = ctxToNode ctx node
             in [ x | x <- (node:lefts ++ rights), matchFn (value x) ]

globalLookup :: (Symbol -> Bool) -> STZipper -> Maybe SymbolTable
globalLookup = (headMay.) . globalFindMatches

localLookup :: (Symbol -> Bool) -> STZipper -> Maybe SymbolTable
localLookup = (headMay.) . localFindMatches
