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
  | TNatType        -- types
  | TFunc 
  | TElse 
  | TThen 
  | TUnit 
  | TLetRec
  | TLet  
  | TSuc  
  | TPrd  
  | TIsZ  
  | TIf   
  | TIn   
  | TAs             -- keywords
  | TVar String     -- variables
  | TZero   
  | TTrue   
  | TFalse          -- constants
  | TSemiColon
  | TAssign 
  | TLParen 
  | TRParen 
  | TArrow
  | TColon          -- symbols
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer s@(x:xs)
  | isAlpha x = lexVar s
  | isSpace x = lexer xs
  | ';' == x  = TSemiColon: rest
  | '=' == x  = TAssign: rest
  | '(' == x  = TLParen: rest
  | ')' == x  = TRParen: rest
  | ':' == x  = TColon: rest
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
      "func" -> TFunc
      "else" -> TElse
      "then" -> TThen
      "unit" -> TUnit
      "letrec"  -> TLetRec
      "let"  -> TLet
      "suc"  -> TSuc
      "prd"  -> TPrd
      "isZero" -> TIsZ
      "if"   -> TIf
      "in"   -> TIn
      "as"   -> TAs
      "Zero" -> TZero
      "True" -> TTrue
      "False"-> TFalse
      _      -> TVar tok

