module Parser.BaseCombinators
    (
    token, simpleToken,
    tokenWP, simpleTokenWP,
    anyType, exactType, nonVoidType,
    anyTypeWP,
    identifier, integer, boolean, float,
    identifierWP, integerWP, booleanWP, floatWP,
    semicolon, lparen, rparen, lbrace, rbrace,
    semicolonWP, lparenWP, rparenWP, lbraceWP, rbraceWP
    )
    where

import Parser.TokStream (Parser)
import qualified Text.Megaparsec as MP
import Tokens
import AST
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))

liftToken :: CivicToken -> WithPos CivicToken
liftToken = WithPos pos pos (-1)
    where pos = MP.initialPos "<input>"

tokenWP :: String -> (CivicToken -> Maybe s) -> Parser (WithPos s)
tokenWP expected fn = MP.token test Set.empty MP.<?> expected
    where test wp@WithPos{value=v} = (<$ wp) <$> fn v

token :: String -> (CivicToken -> Maybe s) -> Parser s
token expected fn = MP.token test Set.empty MP.<?> expected
    where test WithPos{value=v} = fn v

nonVoidType :: Parser Type
nonVoidType = token "a non-void type" toNVType
    where toNVType (Type TVoid) = Nothing
          toNVType (Type t) = Just t
          toNVType _ = Nothing

anyType :: Parser Type
anyType = token "a type" toType

anyTypeWP :: Parser (WithPos Type)
anyTypeWP = tokenWP "a type" toType

exactType :: Type -> Parser Type
exactType t = token (show t) exactType'
    where exactType' t' | t' == Type t = Just t
                        | otherwise = Nothing

simpleToken :: CivicToken -> Parser CivicToken
simpleToken ct = MP.token test (Set.singleton . MP.Tokens . nes . liftToken $ ct)
    where
        test (WithPos _ _ _ x) =
          if x == ct
             then Just x
             else Nothing
        nes x = x :| []

simpleTokenWP :: CivicToken -> Parser (WithPos CivicToken)
simpleTokenWP ct = MP.token test (Set.singleton . MP.Tokens . nes . liftToken $ ct)
    where
        test wp@(WithPos _ _ _ x) =
          if x == ct
             then Just wp
             else Nothing
        nes x = x :| []

identifierWP :: Parser (WithPos String)
identifierWP = MP.token test Set.empty MP.<?> "identifier"
    where test wp@(WithPos _ _ _ (Identifier s)) = Just wp{value = s}
          test WithPos {} = Nothing

identifier :: Parser String
identifier = MP.token test Set.empty MP.<?> "identifier"
    where test WithPos{value=Identifier s} = Just s
          test WithPos {} = Nothing

integer :: Parser Int
integer = token "Integer" toInt

integerWP :: Parser (WithPos Int)
integerWP = tokenWP "Integer" toInt

boolean :: Parser Bool
boolean = token "Boolean" toBool

booleanWP :: Parser (WithPos Bool)
booleanWP = tokenWP "Boolean" toBool

float :: Parser Float
float = token "Float" toFloat

floatWP :: Parser (WithPos Float)
floatWP = tokenWP "Float" toFloat

semicolon, lparen, rparen, lbrace, rbrace :: Parser CivicToken
semicolonWP, lparenWP, rparenWP, lbraceWP, rbraceWP :: Parser (WithPos CivicToken)

semicolon = simpleToken Semicolon
semicolonWP = simpleTokenWP Semicolon

lparen = simpleToken LeftParen
rparen = simpleToken RightParen

lparenWP = simpleTokenWP LeftParen
rparenWP = simpleTokenWP RightParen

lbrace = simpleToken LeftBrace
rbrace = simpleToken RightBrace

lbraceWP = simpleTokenWP LeftBrace
rbraceWP = simpleTokenWP RightBrace
