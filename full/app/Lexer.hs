module Lexer where

import Def

import Data.Char
  ( isAlpha
  , isDigit
  , isSpace 
  )

data Token
  = TUnitType
  | TBoolType
  | TRefType
  | TNatType        -- types
  | TElse 
  | TThen 
  | TUnit 
  | TFun 
  | TRec
  | TLet  
  | TSuc  
  | TPrd  
  | TIsZ  
  | TRef
  | TIf   
  | TIn   
  | TAs             -- keywords
  | TId String      -- variables
  | TInt Int
  | TZero   
  | TTrue   
  | TFalse          -- constants
  | TSemiColon
  | TBind 
  | TLParen 
  | TRParen 
  | TLBrack 
  | TRBrack 
  | TArrow
  | TAssign
  | TColon
  | TComma
  | TExc
  | TDot            -- symbols
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer s@(x:xs)
  | isDigit x = lexInt s
  | isAlpha x = lexVar s
  | isSpace x = lexer xs
  | '_' == x  = TId [x]: rest
  | ';' == x  = TSemiColon: rest
  | '=' == x  = TBind: rest
  | ')' == x  = TRParen: rest
  | '{' == x  = TLBrack: rest
  | '}' == x  = TRBrack: rest
  | '!' == x  = TExc : rest
  | '.' == x  = TDot : rest
  | ',' == x  = TComma : rest
  | '(' == x  = 
    case head xs of
      '*' -> lexComment 1 (tail xs)
      _   -> TLParen: rest
  | ':' == x  
    = case head xs of
      '=' -> TAssign: lexer (tail xs)
      _   -> TColon : rest
  | '-' == x  
    = case head xs of
      '>' -> TArrow: lexer (tail xs)
      _   -> error "Expect '->'"
  | '/' == x
    = let (_, r) = span (/= '\n') xs
    in case head xs of
      '/' -> lexer r
      _   -> error "Expect '//'"  
  where rest = lexer xs
lexer s = error $ "Lexer error: " ++ s

lexVar :: String -> [Token]
lexVar s = let 
  (tok, xs) = span (\x -> isAlpha x || isDigit x) s
  rest = lexer xs in 
    (: rest) $ case tok of
      "Unit" -> TUnitType
      "Bool" -> TBoolType
      "Nat"  -> TNatType
      "Ref"  -> TRefType
      "else" -> TElse
      "then" -> TThen
      "unit" -> TUnit
      "fun"  -> TFun
      "rec"  -> TRec
      "let"  -> TLet
      "suc"  -> TSuc
      "prd"  -> TPrd
      "ref"  -> TRef
      "isZero" -> TIsZ
      "if"   -> TIf
      "in"   -> TIn
      "as"   -> TAs
      "Zero" -> TZero
      "True" -> TTrue
      "False"-> TFalse
      _      -> TId tok

lexInt :: String -> [Token]
lexInt s = 
  let (tok, xs) = span isDigit s
      rest = lexer xs
      in TInt (read tok): rest

lexComment :: Int -> String -> [Token]
lexComment 0 = lexer
lexComment depth = go where
  go :: String -> [Token]
  go [] = []
  go (x:xs)
    | '*' == x = 
      case head xs of
        ')' -> lexComment (depth - 1) $ tail xs
        _   -> go $ tail xs
    | '(' == x =
      case head xs of
        '*' -> lexComment (depth + 1) $ tail xs
        _   -> go $ tail xs
    | otherwise = go xs
