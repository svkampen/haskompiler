{-# LANGUAGE PartialTypeSignatures, NamedFieldPuns, GADTs, TypeApplications, RankNTypes, DuplicateRecordFields, MultiParamTypeClasses, ScopedTypeVariables #-}
module Parser.Parser where

import AST
import qualified Tokens as T
import Parser.TokStream
import Data.Data hiding (typeRep, Prefix)
import Type.Reflection
import Parser.BaseCombinators
import Text.Megaparsec (try, (<?>), runParserT)
import qualified Text.Megaparsec as MP
import Control.Arrow (left)
import Control.Applicative (Alternative)
import Control.Monad.Reader
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup (sconcat)
import Metadata
import Errors (ComponentError(..))
import Parser.Configuration
import Data.Default

getCurrentLocation :: Parser (WithPos ())
getCurrentLocation = do
    MP.State{stateInput} <- MP.getParserState
    return . void . head . nextTokens $ stateInput

getPreviousLocation :: Parser (WithPos ())
getPreviousLocation = do
    MP.State{stateInput} <- MP.getParserState
    return . void . last . previousTokens $ stateInput

wrapInSpan :: Parser (WithoutSpan a) -> Parser a
wrapInSpan parser = do
    loc <- getCurrentLocation
    f <- parser
    loc' <- getPreviousLocation
    return . f $ spanBetween loc loc'

lbinop, rbinop :: HasSpan a => (a -> a -> SourceSpan -> a) -> T.CivicToken -> Operator Parser a

binop :: (HasSpan a, HasSpan b) => (Parser (a -> b -> t1) -> t2) -> (a -> b -> SourceSpan -> t1) -> T.CivicToken -> t2
binop btype con tok = btype $ simpleToken tok *> do
    return (\e1 e2 -> con e1 e2 (getSpan e1 <> getSpan e2))

lbinop = binop InfixL
rbinop = binop InfixR

unary :: HasSpan a => (Parser (a -> t1) -> t2) -> (a -> SourceSpan -> t1) -> T.CivicToken -> t2
unary utype con tok = utype $ do
    loc <- getCurrentLocation
    _ <- simpleToken tok
    loc' <- getPreviousLocation
    return (\expr -> con expr (spanBetween loc loc' <> getSpan expr))

prefix, postfix :: HasSpan a => (a -> SourceSpan -> a) -> T.CivicToken -> Operator Parser a
prefix = unary Prefix
postfix = unary Postfix

parenthesized, braced :: Parser a -> Parser a
parenthesized = between lparen rparen
braced        = between lbrace rbrace

exists :: Alternative f => f b -> f Bool
exists p = (True <$ p) <|> pure False

table :: [[Operator Parser Expr]]
table = [
    [prefix Not T.Not, prefix Neg T.Minus],
    [lbinop Mul T.Star, lbinop Div T.Slash, lbinop Mod T.Percent],
    [lbinop Add T.Plus, lbinop Sub T.Minus],
    [lbinop LessEqual T.LessThanEqual, lbinop LessThan T.LessThan, lbinop GreaterEqual T.GreaterThanEqual, lbinop GreaterThan T.GreaterThan],
    [lbinop Equal T.Equal, lbinop NotEqual T.NotEqual],
    [lbinop And T.And],
    [lbinop Or T.Or]
        ]

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

constant :: Parser Expr
constant = choice $ wrapInSpan <$>
                  [IntConst <$> integer,
                   BoolConst <$> boolean,
                   FloatConst <$> float]

unreachable :: a
unreachable = undefined

antiquotation :: Parser Expr
antiquotation = wrapInSpan $ do
    ty <- choice (simpleToken <$> [T.AntiInt, T.AntiBool, T.AntiExpr])
    id <- identifier
    return $ case ty of
      T.AntiInt -> AntiInt id
      T.AntiBool -> AntiBool id
      T.AntiExpr -> AntiExpr id
      _ -> unreachable

typecast :: Parser Expr
typecast = wrapInSpan $ Cast <$> parenthesized nonVoidType <*> expr

term :: Parser Expr
term = do
    inQuoter <- lift . reader $ qqSupport

    let choices = [try $ parenthesized expr,
                   try $ typecast,
                   try . wrapInSpan $ call CallExpr,
                   constant, wrapInSpan (Var <$> identifier)]

    if inQuoter
       then choice choices <|> antiquotation <?> "term"
       else choice choices <?> "term"

vardecl :: Parser VarDecl
vardecl = wrapInSpan $ VarDecl <$> nonVoidType <*> identifier <*> optional (simpleToken T.Let *> expr) <* semicolon
--
-- Comma-separated exprs, e.g. "1, 3+6, a, (b/c)"
exprs :: Parser [Expr]
exprs = ((:) <$> expr <*> many (simpleToken T.Comma *> expr)) <|> pure []

-- Statements

-- Do/while loop
dowhile :: Parser Statement
dowhile = simpleToken T.Do *> do
    block <- braced (many statement)
    _ <- simpleToken T.While
    condition <- parenthesized expr
    return $ While LTDoWhile condition block

-- While loop
while :: Parser Statement
while = simpleToken T.While *> do
    condition <- parenthesized expr
    block <- braced (many statement)
    return $ While LTWhile condition block

mkQ1 :: forall f b q. (Typeable f, Typeable b) => (forall x. f x -> q) -> (b -> q) -> (b -> q)
mkQ1 h =
  case typeRep @b of
    App g _ ->
      case eqTypeRep g (typeRep @f) of
        Just HRefl -> const h
        _ -> id
    _ -> id

getWithPos :: Data a => a -> Maybe (T.WithPos ())
getWithPos = mkQ1 (Just . void) (const Nothing)

withPos :: Data a => a -> T.WithPos a
withPos x = x <$ sconcat ((NE.fromList . catMaybes) (gmapQ getWithPos x))

if_ :: Parser Statement
if_ = simpleToken T.If *> do
    condition <- parenthesized expr
    if_block <- braced (many statement)
    else_block <- option [] (simpleToken T.Else *> braced (many statement))
    return $ If condition if_block else_block

return_ :: Parser Statement
return_ = Return <$> (simpleToken T.Return *> optional expr)

assignment :: Parser Statement
assignment = Assign <$> identifier <* simpleToken T.Let <*> expr

call :: (String -> [Expr] -> a) -> Parser a
call con = con <$> identifier <*> parenthesized exprs

-- | Any statement
statement :: Parser Statement
statement = let nosemis = [if_, while]
                -- call is combined with try as a call and an assignment start the same way.
                semis = [try (call CallStmt), assignment, return_, dowhile]
             in choice (nosemis ++ ((<* semicolon) <$> semis)) <?> "statement"

-- | A (braced) function body
funbody :: Parser Funbody
funbody = braced $ FB <$> many vardecl <*> many statement

-- | A parameter declaration (e.g. "int a")
param :: Parser Param
param = Param <$> anyType <*> identifier

-- | Comma-separated parameter declarations (e.g. "int a, float b")
params :: Parser [Param]
params = ((:) <$> param <*> many (simpleToken T.Comma *> param)) <|> pure []

-- | A global variable declaration
global :: Parser Global
global = wrapInSpan $ Global <$> exists (simpleToken T.Extern) <*> anyType <*> identifier <*> optional (simpleToken T.Let *> expr) <* semicolon

function :: Parser Function
function = wrapInSpan $ do
    visibility <- optional (simpleToken T.Export <|> simpleToken T.Extern)

    let fn = case visibility of {
      Just T.Export -> Function Export;
      Just T.Extern -> Function Extern;
      Just _ -> undefined;
      Nothing -> Function Internal
    }

    decl <- fn <$> anyType <*> identifier <*> parenthesized params

    case visibility of
      Just T.Extern -> decl Nothing <$ semicolon
      _ -> decl . Just <$> funbody

decls :: Parser [Decl]
decls = many (FunctionDecl <$> try function <|> GlobalDecl <$> global)

type InputName = String

run :: Parser a -> InputName -> String -> Either ComponentError a
run = runWithConfig def

-- | Using configuration `config`, run parser `p` on the given input after tokenization.
runWithConfig :: ParseConfiguration -> Parser a -> InputName -> String -> Either ComponentError a
runWithConfig config p inputName input = do
    tokStream <- left (TokenizerError inputName) $ mkStream inputName input
    left ParserError $ runReader (runParserT p inputName tokStream) config
