{-# LANGUAGE ScopedTypeVariables #-}
module Compile where

import Data.Map
  ( Map
  , lookup
  , insert
  , member
  , empty
  )
import Data.Maybe 
  ( fromJust
  )
import Def
import Lexer

type NamingContext = [Name];

type TypingContext = Map String FType;

appF:: FType -> FType -> FType
appF f@(a :=> b) c
  | a == c = b
  | otherwise = error $ "Applying " ++ show f ++ " to " ++ show c
appF a _ = error $ show a ++ " is not function"

typecheck :: FTerm -> FType
typecheck = typecheckF empty

typecheckF:: TypingContext -> FTerm -> FType
typecheckF ctx = go where
  go :: FTerm -> FType
  go (VarF v)
    | member v ctx = fromJust $ Data.Map.lookup v ctx
    | otherwise = error $ "Var " ++ v ++ " not bounded"
  go (AppF m n) = appF (go m) (go n)
  go (AbsF v t n) = t :=> typecheckF (insert v t ctx) n
  go (SucF m) = if go m == NatTypeF
    then NatTypeF 
    else error "Applying Suc to non Nat"
  go (PrdF m) = if go m == NatTypeF
    then NatTypeF
    else error "Applying Prd to non Nat"
  go (IsZF m) = if go m == NatTypeF
    then BoolTypeF
    else error "Applying IsZero to non Nat"
  go ZeroF  = NatTypeF
  go TrueF  = BoolTypeF
  go FalseF = BoolTypeF
  go (IteF t f e)
    | go e /= BoolTypeF = error "If condition should be Bool"
    | go t /= go f = error "If branch should have the same type"
    | otherwise = go t
  go (LinF v m n)
    = typecheckF (insert v (go m) ctx) n
  go UnitF = UnitTypeF
  go (SeqF m n)
    | go m == UnitTypeF = go n
    | otherwise = error "Sequential should yield Unit"
  go (AscF tm tp)
    | go tm == tp = tp
    | otherwise   = error $
      "Casting " ++ show (go tm)
      ++ " to "  ++ show tp
  go (LriF v t m n) = 
    if typecheckF (insert v t ctx) m == t
      then typecheckF (insert v t ctx) n
      else error "letrec should have type T -> T"
  go (FixF m) = case go m of
    (a :=> b) -> if a == b
      then a
      else error "Fix should have type T -> T"
    _ ->   error "Fix should have type T -> T"
  go (FldF fs) = FldTypeF fs'
    where fs' = (go <$>) <$> fs
  go (AccF m f) = case go m of
    (FldTypeF fs) ->
      let result = Prelude.lookup f fs
      in case result of
        Nothing -> error "Field not found"
        Just t  -> t
    _ -> error "Accessing non Struct field"

-- compile Types
compType :: FType -> RType
compType UnitTypeF = UnitTypeR
compType BoolTypeF = BoolTypeR
compType NatTypeF  = NatTypeR
compType (a :=> b) = a' :-> b'
  where a' = compType a
        b' = compType b
compType (FldTypeF fs) = FldTypeR fs'
  where fs' = (compType <$>) <$> fs

-- De Bruijn form
indexOf:: forall a. (Eq a, Show a) => a -> [a] -> Int
indexOf e = go where
  go :: [a] -> Int
  go [] = error $ show e ++ " not bounded"
  go (x:xs)
    | x == e = 0
    | otherwise = 1 + go xs

compile :: DTerm -> RTerm
compile = rewrite []

-- compile Terms
rewrite :: NamingContext -> DTerm -> RTerm
rewrite ctx = go where
  go :: DTerm -> RTerm
  go (VarF x)   = VarR $ indexOf x ctx
  go (AppF m n) = go m `AppR` go n
  go (AbsF v t m) = AbsR m'
    where m' = rewrite (v:ctx) m
  go ZeroF  = ZeroR
  go TrueF  = TrueR
  go UnitF  = UnitR
  go FalseF = FalseR
  go (SucF m) = SucR $ go m
  go (PrdF m) = PrdR $ go m
  go (IsZF m) = IsZR $ go m
  go (IteF t f e) = IteR (go t) (go f) (go e)
  go (AscF tm tp) = AscR tm' tp'
    where tm' = go tm
          tp' = compType tp
  go (FixF m) = FixR m'
    where m' = go m
  go (FldF fs) = FldR fs'
    where fs' = (go <$>) <$> fs
  go (AccF m f) = AccR (go m) f
  go e = error $ show e ++ " DTerm shouldn't contain these"

-- desugar
desugar :: TypingContext -> FTerm -> DTerm
desugar ctx = go where
  check :: FTerm -> FType
  check = typecheckF ctx

  go :: FTerm -> DTerm
  go (AppF m n)
    = AppF m' n' where 
      m' = go m
      n' = go n
  go (AbsF v t m) 
    = AbsF v t m' where
      m' = desugar ctx' m
      ctx' = insert v t ctx
  go (IteF t f e) 
    = IteF t' f' e' where
      t' = go t
      f' = go f
      e' = go e
  go (PrdF m) 
    = PrdF m' where 
      m' = go m
  go (SucF m) 
    = SucF m' where 
      m' = go m
  go (IsZF m) 
    = IsZF m' where 
      m' = go m
  go (AscF m t) 
    = AscF m' t where 
      m' = go m
  go (LriF v t@(t' :=> _) m n) 
    = AppF (AbsF v t n') (FixF (AbsF v t m')) 
    where m' = desugar (insert v t ctx) m
          n' = desugar (insert v t ctx) n
  go (LinF v m n)
    = AppF (AbsF v t n') m' where
      t = check m
      m' = go m
      n' = desugar ctx' n
      ctx' = insert v t ctx
  go (SeqF m n)
    | check m == UnitTypeF = AppF (AbsF "_" UnitTypeF n) m
    | otherwise = error "Sequencing should yield Unit"
  go x = x
