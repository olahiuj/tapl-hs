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
  | TRec
  | TLet  
  | TSuc  
  | TPrd  
  | TIsZ  
  | TIf   
  | TIn   
  | TAs             -- keywords
  | TId String      -- variables
  | TZero   
  | TTrue   
  | TFalse          -- constants
  | TSemiColon
  | TAssign 
  | TLParen 
  | TRParen 
  | TLBrack 
  | TRBrack 
  | TArrow
  | TColon
  | TComma
  | TDot            -- symbols
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer s@(x:xs)
  | isAlpha x = lexVar s
  | isSpace x = lexer xs
  | ';' == x  = TSemiColon: rest
  | '=' == x  = TAssign: rest
  | '(' == x  = 
    case head xs of
      '*' -> lexComment 1 (tail xs)
      _   -> TLParen: rest
  | ')' == x  = TRParen: rest
  | '{' == x  = TLBrack: rest
  | '}' == x  = TRBrack: rest
  | ':' == x  = TColon : rest
  | ',' == x  = TComma : rest
  | '.' == x  = TDot : rest
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
      "rec"  -> TRec
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
      _      -> TId tok

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
      
    
