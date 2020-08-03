{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}
module AST
    (
        WithoutSpan,

        Decl(..),

        -- Type
        Type(..),

        -- Declaration visibility (extern, export)
        Visibility(..),

        -- Variable declarations
        Global(..), VarDecl(..),

        -- Functions
        Function(..), Funbody(..), Param(..),

        -- Statements
        Statement(..), LoopType(..),

        -- Expressions
        Expr(..)
    )
    where

import Prelude hiding (LT, GT, EQ, span)
import Metadata (SourceSpan, HasSpan)
import GHC.Generics
import Data.Typeable
import Data.Data

type WithoutSpan a = (SourceSpan -> a)

data LoopType = LTWhile | LTDoWhile deriving (Eq, Enum, Bounded, Show, Data, Typeable)

data Type = TInt | TVoid | TBool | TFloat deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show Type where
  show TInt = "int"
  show TVoid = "void"
  show TBool = "bool"
  show TFloat = "float"

-- | A top-level declaration.
data Decl = GlobalDecl Global | FunctionDecl Function deriving (Show, Data, Typeable, Generic, HasSpan)

-- | The visibility of a declaration.
data Visibility = Export | Extern | Internal deriving (Eq, Show, Typeable, Data, Enum, Bounded)

-- | A Civic expression.
data Expr
  = Var String SourceSpan
  | IntConst Int SourceSpan
  | FloatConst Float SourceSpan
  | BoolConst Bool SourceSpan
  | Not Expr SourceSpan
  | Neg Expr SourceSpan
  | Add Expr Expr SourceSpan
  | Sub Expr Expr SourceSpan
  | Mul Expr Expr SourceSpan
  | Div Expr Expr SourceSpan
  | Mod Expr Expr SourceSpan
  | LessThan Expr Expr SourceSpan
  | LessEqual Expr Expr SourceSpan
  | GreaterEqual Expr Expr SourceSpan
  | GreaterThan Expr Expr SourceSpan
  | Equal Expr Expr SourceSpan
  | NotEqual Expr Expr SourceSpan
  | And Expr Expr SourceSpan
  | Or Expr Expr SourceSpan
  | Cast Type Expr SourceSpan
  | CallExpr String [Expr] SourceSpan
  | AntiExpr String SourceSpan
  | AntiInt String SourceSpan
  | AntiBool String SourceSpan
  deriving (Eq, Show, Typeable, Data, Generic, HasSpan)

-- | A local variable declaration.
data VarDecl = VarDecl { vtype :: Type, name :: String, initializer :: Maybe Expr, __span :: SourceSpan } deriving (Show, Eq, Data, Typeable, Generic, HasSpan)

-- | A global variable declaration.
data Global = Global { isExtern :: Bool, varType :: Type, name :: String, initializer :: Maybe Expr, __span :: SourceSpan } deriving (Show, Typeable, Data, Generic, HasSpan)

-- | Function parameter declaration.
data Param = Param Type String deriving (Show, Data)

-- | A function declaration, optionally with body
data Function = Function {
    fnVisibility :: Visibility,
    fnReturnType :: Type,
    fnName :: String,
    fnParams :: [Param],
    fnBody :: Maybe Funbody,
    span :: SourceSpan
} deriving (Show, Typeable, Data, Generic, HasSpan)

-- | A function body.
data Funbody = FB {
    vardecls :: [VarDecl],
    stmts :: [Statement]
} deriving (Show, Typeable, Data)

-- | A statement.
data Statement = Assign String Expr
               | CallStmt String [Expr]
               | If Expr [Statement] [Statement]
               | While LoopType Expr [Statement]
               | For VarDecl Expr Expr (Maybe Expr) [Statement]
               | Return (Maybe Expr)
                   deriving (Show, Data, Typeable)

