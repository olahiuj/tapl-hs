module Exec where

import Term

run :: Term -> Term
run = until isValue reduce

reduce :: Term -> Term
reduce (Var_ v) = Var_ v
reduce (App_ m n)
  | isValue m && isValue n = sub (getPar m) n (getBody m)
  | isValue m = App_ m $ reduce n
  | otherwise = (`App_` n) $ reduce m

reduce (Prd_ (Suc_ m)) = m
reduce (Prd_ Zero_) = Zero_
reduce (Prd_ m) = Prd_ $ reduce m

reduce (Suc_ m) = Suc_ $ reduce m

reduce (IsZ_ Zero_) = True_
reduce (IsZ_ (Suc_ _)) = False_
reduce (IsZ_ m) = IsZ_ $ reduce m

reduce (Ite_ t _ True_ ) = t
reduce (Ite_ _ f False_) = f
reduce (Ite_ t f e) = Ite_ t f $ reduce e

reduce (Seq_ Unit_ n) = n
reduce (Seq_ m n) = Seq_ (reduce m) n 

reduce (Lin_ v m n) = sub v m n

reduce (Asc_ tm tp)
  | isValue tm = tm
  | otherwise  = Asc_ (reduce tm) tp

reduce x = x
