{-# LANGUAGE ScopedTypeVariables #-}
module Eval where
-- Runtime representations:
--  1. No type information
--  2. All variables have different names
--  3. Constructors must end with underscore "_" 

import Def
import Data.Maybe (fromJust)

eval :: RTerm -> RTerm
eval = until isValue reduce

reduce :: RTerm -> RTerm
reduce (VarR v) = VarR v
reduce (AppR m n) = case m of
  (AbsR l) -> if isValue n
    then shift 0 (-1) $ sub 0 n l
    else AppR m (reduce n)
  _ -> AppR (reduce m) n
reduce (PrdR (SucR m)) = m
reduce (PrdR ZeroR) = ZeroR
reduce (PrdR m) = PrdR $ reduce m
reduce (SucR m) = SucR $ reduce m
reduce (IsZR ZeroR) = TrueR
reduce (IsZR (SucR _)) = FalseR
reduce (IsZR m) = IsZR $ reduce m
reduce (IteR t _ TrueR ) = t
reduce (IteR _ f FalseR) = f
reduce (IteR t f e) = IteR t f $ reduce e
reduce (AscR tm tp)
  | isValue tm = tm
  | otherwise  = AscR (reduce tm) tp
reduce m@(FixR (AbsR n)) = sub 0 m n
reduce (FixR m) = FixR (reduce m)
reduce (FldR fs) = FldR fs'
  where fs' = lhs ++ [reduce <$> redex] ++ tail rhs
        redex = head rhs
        (lhs, rhs) = span (isValue . snd) fs
reduce (AccR m f)
  | isValue m = case m of
    (FldR fs) -> fromJust $ lookup f fs
    _ -> error "Accessing non Fld"
  | otherwise = AccR (reduce m) f
reduce x = x

isValue :: RTerm -> Bool
isValue ZeroR  = True
isValue TrueR  = True
isValue FalseR = True
isValue (SucR m) = isValue m
isValue (PrdR m) = False
isValue (IsZR m) = isValue m
isValue AbsR {} = True
isValue UnitR  = True
isValue (FldR fs) = all (isValue . snd) fs
isValue _ = False

shift :: Int -> Int -> RTerm -> RTerm
shift d c = go where
  go :: RTerm -> RTerm
  go v@(VarR i)
    | i >= c = VarR $ i + d
    | otherwise = v
  go (AppR m n) = AppR m' n'
    where m' = go m
          n' = go n
  go (AbsR n) = AbsR n'
    where n' = shift d (c + 1) n
  go ZeroR = ZeroR
  go TrueR = TrueR
  go UnitR = UnitR
  go FalseR= FalseR
  go (SucR m) = SucR m' where m' = go m
  go (PrdR m) = PrdR m' where m' = go m
  go (IsZR m) = IsZR m' where m' = go m
  go (IteR t f e) = IteR t' f' e'
    where t' = go t
          f' = go f
          e' = go e
  go (AscR tm tp) = AscR (go tm) tp
  go (FixR m) = FixR (go m)
  go (FldR fs) = FldR $ (go <$>) <$> fs
  go (AccR f v) = AccR (go f) v

-- x->l->t->t[l/x]
sub :: Int -> RTerm -> RTerm -> RTerm
sub x l = go where
  go :: RTerm -> RTerm
  go (VarR i) = if x == i
    then l
    else VarR i
  go (AppR m n) = AppR m' n'
    where m' = go m
          n' = go n
  go (AbsR n) = AbsR n'
    where n' = sub (x + 1) (shift 0 1 l) n
  go ZeroR = ZeroR
  go TrueR = TrueR
  go UnitR = UnitR
  go FalseR= FalseR
  go (SucR m) = SucR $ go m 
  go (FldR fs) = FldR $ (go <$>) <$> fs
  go (AccR f v) = AccR (go f) v
  go (PrdR m) = PrdR $ go m
  go (IsZR m) = IsZR $ go m
  go (IteR t f e) = IteR (go t) (go f) (go e)
  go (AscR tm tp) = AscR (go tm) tp
  go (FixR m) = FixR (go m)
