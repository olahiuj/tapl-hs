module Lexer
where

import Term

import Data.Char
  ( isAlpha
  , isDigit
  , isSpace 
  )

type Name = String

data Token
-- types
  = TUnitType
  | TBoolType
  | TNatType 
-- keywords
  | TFunc 
  | TElse 
  | TThen 
  | TUnit 
  | TLet  
  | TSuc  
  | TPrd  
  | TIsZ  
  | TIf   
  | TIn   
-- variables
  | TVar String
-- constants
  | TZero   
  | TTrue   
  | TFalse  
-- symbols
  | TSemiColon
  | TAssign 
  | TLParen 
  | TRParen 
  | TArrow
  | TColon  
  deriving (Show, Eq);

data Term'
  = Var' Name     
  | App' Term' Term'
  | Abs' Name  Type' Term'
  | Ite' Term' Term' Term'
  | Lin' Name  Term' Term'
  | Seq' Term' Term'
  | Prd' Term'
  | Suc' Term'    
  | IsZ' Term'    
  | False'        
  | True'         
  | Zero'         
  | Unit'
  deriving (Show, Eq);

data Type'
  = UnitType'
  | BoolType'
  | NatType'
  | Type' :=> Type'
  deriving (Show, Eq);

compTerm :: Term' -> Term
compTerm (Var' v) = Var_ v
compTerm (App' m n) = App_ m' n'
  where m' = compTerm m
        n' = compTerm n
compTerm (Abs' v t m) = Abs_ v t' m'
  where t' = compType t
        m' = compTerm m
compTerm (Ite' t f e) = Ite_ t' f' e'
  where t' = compTerm t
        f' = compTerm f
        e' = compTerm e
compTerm (Lin' v m n) = Lin_ v m' n'
  where m' = compTerm m
        n' = compTerm n
compTerm (Seq' m n) = Seq_ m' n'
  where m' = compTerm m
        n' = compTerm n
compTerm (Prd' m) = Prd_ m'
  where m' = compTerm m
compTerm (Suc' m) = Suc_ m'
  where m' = compTerm m
compTerm (IsZ' m) = IsZ_ m'
  where m' = compTerm m
compTerm False' = False_
compTerm True' = True_
compTerm Zero' = Zero_
compTerm Unit' = Unit_

compType :: Type' -> Type
compType UnitType' = UnitType_
compType BoolType' = BoolType_
compType NatType'  = NatType_
compType (a :=> b) = a' :-> b'
  where a' = compType a
        b' = compType b

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
      "let"  -> TLet
      "suc"  -> TSuc
      "prd"  -> TPrd
      "isZero" -> TIsZ
      "if"   -> TIf
      "in"   -> TIn
      "Zero" -> TZero
      "True" -> TTrue
      "False"-> TFalse
      _      -> TVar tok


