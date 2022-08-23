{-# LANGUAGE ScopedTypeVariables #-}
module Eval where
-- Runtime representations:
--  1. No type information
--  2. All variables have different names
--  3. Constructors must end with underscore "_" 

import Def
import Runtime
import Data.Maybe (fromJust)
import Data.Tuple (swap)

-- continuation
type Cont = RTerm -> RTerm

eval :: Heap -> RTerm -> Conf
eval = curry $ until (isValue . snd) (reduce id)

reduce :: Cont -> Conf -> Conf
reduce con = (con <$>) <$> go where
  go :: Conf -> Conf
  go (h, term) = case term of
    VarR v -> (h, VarR v)
    AppR f@(AbsR m) n ->
      if isValue n  
        then (h, shift 0 (-1) (sub 0 n m))
        else reduce (AppR f) (h, n)
    AppR m n -> reduce (`AppR` n) (h, m)
    PrdR (SucR m) -> (h, m)
    PrdR ZeroR -> (h, ZeroR)
    PrdR m -> reduce PrdR (h, m)
    SucR m -> reduce SucR (h, m)
    IsZR ZeroR -> (h, TrueR)
    IsZR (SucR _) -> (h, FalseR)
    IsZR m -> reduce IsZR (h, m)
    IteR t _ TrueR  -> (h, t)
    IteR _ f FalseR -> (h, f)
    IteR t f e -> reduce (IteR t f) (h, e)
    AscR tm tp ->
      if isValue tm
        then (h, tm)
        else reduce (`AscR` tp) (h, tm)
    m@(FixR (AbsR n)) -> (h, sub 0 m n)
    FixR m -> reduce FixR (h, m)
    FldR fs -> reduce (\x -> FldR (lhs ++ ((n, x):rhs))) (h, redex)
      where (lhs, (n, redex):rhs) = span (isValue . snd) fs
    AccR m f ->
      if isValue m
        then case m of
          (FldR fs) -> (h, fromJust $ lookup f fs)
          _ -> error "Accessing non Fld"
        else reduce (`AccR` f) (h, m)
    RefR m -> if isValue m
      then swap (alloc m h)
      else reduce RefR (h, m)
    DrfR (LocR l) -> (h, load l h)
    DrfR m -> reduce DrfR (h, m)
    
    AssR (LocR l) n ->
      if isValue n
      then (store l n h, UnitR)
      else reduce (AssR (LocR l)) (h, n)
    AssR m n -> reduce (`AssR` n) (h, m)
    m -> (h, m)

isValue :: RTerm -> Bool
isValue ZeroR  = True
isValue TrueR  = True
isValue FalseR = True
isValue (SucR m) = isValue m
isValue (PrdR _) = False
isValue (IsZR m) = isValue m
isValue AbsR {} = True
isValue UnitR  = True
isValue (FldR fs) = all (isValue . snd) fs
isValue (LocR _) = True
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
  go (RefR m) = RefR (go m)
  go (DrfR m) = DrfR (go m)
  go (AssR m n) = AssR (go m) (go n)
  go (LocR l) = LocR l

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
  go (RefR m) = RefR (go m)
  go (DrfR m) = DrfR (go m)
  go (AssR m n) = AssR (go m) (go n)
  go (LocR l) = LocR l
