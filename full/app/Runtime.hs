-- naive runtime for fullSTLC
-- only allocates, no gc
module Runtime where

import Prelude hiding (lookup)
import Def
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Map (Map, findMin, insert, lookup, empty)

type Heap = Map Store RTerm

type Conf = (Heap, RTerm)

alloc :: RTerm -> Heap -> (RTerm, Heap)
alloc m = go where
  go :: Heap -> (RTerm, Heap)
  go h
    | null h = (LocR 0, insert 0 m empty)
    | otherwise = (LocR $ succ l, h')
    where l = fst $ findMin h
          h' = insert (succ l) m h

load :: Store -> Heap -> RTerm
load l h = fromJust $ lookup l h

store :: Store -> RTerm -> Heap -> Heap
store = insert
