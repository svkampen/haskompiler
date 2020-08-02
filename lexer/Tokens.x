{
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, DeriveFunctor #-}
module Tokens (
    WithPos(..), CivicToken(..),
    toInt, toFloat, toType, toIdentifier, toBool, alexMonadScan, runAlex, getFilename, setFilename, wpReplaceVal
    ) where

import qualified AST

import Text.Megaparsec (mkPos)
import Data.Data
import Metadata
}

%wrapper "monadUserState"

tokens :-

  $white+   ;
  "["       { justToken LeftBracket }
  "]"       { justToken RightBracket }
  "{"       { justToken LeftBrace }
  "}"       { justToken RightBrace }
  "("       { justToken LeftParen }
  ")"       { justToken RightParen }
  ","       { justToken Comma }
  ";"       { justToken Semicolon }
  "-"       { justToken Minus }
  "+"       { justToken Plus }
  "*"       { justToken Star }
  "/"       { justToken Slash }
  "%"       { justToken Percent }
  "!"       { justToken Not }
  "<="      { justToken LessThanEqual }
  "<"       { justToken LessThan }
  ">="      { justToken GreaterThanEqual }
  ">"       { justToken GreaterThan }
  "=="      { justToken Equal }
  "!="      { justToken NotEqual }
  "&&"      { justToken And }
  "||"      { justToken Or }
  "="       { justToken Let }
  "true"    { justToken TrueVal }
  "false"   { justToken FalseVal }
  "void"    { justToken (Type AST.TVoid) }
  "bool"    { justToken (Type AST.TBool) }
  "int"     { justToken (Type AST.TInt) }
  "float"   { justToken (Type AST.TFloat) }
  "return"  { justToken Return }
  "for"     { justToken For }
  "do"      { justToken Do }
  "while"   { justToken While }
  "if"      { justToken If }
  "else"    { justToken Else }
  "extern"  { justToken Extern }
  "export"  { justToken Export }
  "$int:"   { justToken AntiInt }
  "$bool:"  { justToken AntiBool }
  "$e:"     { justToken AntiExpr }

  [A-Za-z][A-Za-z0-9'\_]*           { posWrap (\s -> Identifier s) }
  [0-9]+                            { posWrap (\s -> Int (read s)) }
  [1-9][0-9]*\.[0-9]*               { posWrap (\s -> Float (read s)) }

{

type AlexUserState = String

alexInitUserState :: String
alexInitUserState = "<unknown input source>"

getFilename :: Alex String
getFilename = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ust)

setFilename :: String -> Alex ()
setFilename fname = Alex $ \s -> Right (s{alex_ust=fname}, ())

posWrap :: (String -> CivicToken) -> AlexInput -> Int -> Alex (WithPos CivicToken)
posWrap f ((AlexPn off ln col), _, _, inp) len = do
    fname <- getFilename
    let str = take len inp
    return $ WithPos {
        startPos = SourcePos fname (mkPos ln) (mkPos col),
        endPos = SourcePos fname (mkPos ln) (mkPos (col + length str - 1)),
        startOffset = 1 + off,
        value = f str
    }

alexEOF = return (WithPos { startPos = pos, endPos = pos, startOffset = 0, value = EOF })
          where pos = (SourcePos "<input>" (mkPos 0) (mkPos 0))

-- | Yield a token with no additional information.
justToken :: CivicToken -> AlexInput -> Int -> Alex (WithPos CivicToken)
justToken = posWrap . const

-- | Tokens for the Civic language.
data CivicToken =
    LeftBracket | RightBracket | LeftBrace | RightBrace | LeftParen | RightParen |
    Comma | Semicolon |
    Minus | Plus | Star | Slash | Percent | Not |
    LessThanEqual | LessThan | GreaterThanEqual | GreaterThan | Equal | NotEqual |
    And | Or |
    Let |
    TrueVal | FalseVal |
    Type AST.Type |
    Return | For | Do | While | If | Else |
    Extern | Export |
    Int Int | Float Float |
    Identifier String | EOF |
    AntiInt | AntiBool | AntiExpr
    deriving (Eq, Ord, Data)

instance Show CivicToken where
    show EOF = "end of input"
    show AntiInt = "int antiquotation"
    show AntiBool = "bool antiquotation"
    show AntiExpr = "expr antiquotation"
    show LeftBracket = "'['"
    show RightBracket = "']'"
    show LeftBrace = "'{'"
    show RightBrace = "'}'"
    show LeftParen = "'('"
    show RightParen = "')'"
    show Comma = "','"
    show Semicolon = "';'"
    show Minus = "'-'"
    show Plus = "'+'"
    show Star = "'*'"
    show Slash = "'/'"
    show Percent = "'%'"
    show Not = "'!'"
    show LessThanEqual = "'<='"
    show LessThan = "'<'"
    show GreaterThanEqual = "'>='"
    show GreaterThan = "'>'"
    show Equal = "'=='"
    show NotEqual = "'!='"
    show And = "'&&'"
    show Or = "'||'"
    show Let = "'='"
    show TrueVal = "'true'"
    show FalseVal = "'false'"
    show (Type AST.TInt) = "'int'"
    show (Type AST.TVoid) = "'void'"
    show (Type AST.TBool) = "'bool'"
    show (Type AST.TFloat) = "'float'"
    show Return = "'return'"
    show For = "'for'"
    show Do = "'do'"
    show While = "'while'"
    show If = "'if'"
    show Else = "'else'"
    show Extern = "'extern'"
    show Export = "'export'"
    show (Int i) = show i
    show (Float f) = show f
    show (Identifier s) = "\"" ++ s ++ "\""

toInt :: CivicToken -> Maybe Int
toInt (Int i) = Just i
toInt _ = Nothing

toBool :: CivicToken -> Maybe Bool
toBool TrueVal = Just True
toBool FalseVal = Just False
toBool _ = Nothing

toFloat :: CivicToken -> Maybe Float
toFloat (Float f) = Just f
toFloat _ = Nothing

toType :: CivicToken -> Maybe AST.Type
toType (Type t) = Just t
toType _ = Nothing

toIdentifier :: CivicToken -> Maybe String
toIdentifier (Identifier s) = Just s
toIdentifier _ = Nothing
}
