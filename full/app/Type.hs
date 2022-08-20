module Type
  ( typecheck
  )
where

import Prelude hiding ( lookup )
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
import Term

type Context = Map String Type;

app_:: Type -> Type -> Type
app_ f@(a :-> b) c
  | a == c = b
  | otherwise = error $ "Applying " ++ show f ++ " to " ++ show c
app_ a _ = error $ show a ++ " is not function"

typecheck :: Term -> Type
typecheck = typecheck' empty

typecheck':: Context -> Term -> Type
typecheck' ctx = go where
  go :: Term -> Type
  go (Var_ v)
    | member v ctx = fromJust $ lookup v ctx
    | otherwise = error $ "Var " ++ v ++ " not bounded"
  go (App_ m n) = app_ (go m) (go n)
  go (Abs_ v t n) = t :-> typecheck' (insert v t ctx) n
  go (Suc_ m) = if go m == NatType_
    then NatType_ 
    else error "Applying Suc to non Nat"
  go (Prd_ m) = if go m == NatType_
    then NatType_
    else error "Applying Prd to non Nat"
  go (IsZ_ m) = if go m == NatType_
    then BoolType_
    else error "Applying IsZero to non Nat"
  go Zero_  = NatType_
  go True_  = BoolType_
  go False_ = BoolType_
  go (Ite_ t f e)
    | go e /= BoolType_ = error "If condition should be Bool"
    | go t /= go f = error "If branch should have the same type"
    | otherwise = go t
  go (Lin_ v m n)
    = typecheck' (insert v (go m) ctx) n
  go Unit_ = UnitType_
  go (Seq_ m n)
    | go m == UnitType_ = go n
    | otherwise = error "Sequential should yield Unit"
  go (Asc_ tm tp)
    | go tm == tp = tp
    | otherwise   = error $
      "Casting " ++ show (go tm)
      ++ " to "  ++ show tp
