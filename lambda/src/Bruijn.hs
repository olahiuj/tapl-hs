module Bruijn where

import Common

redex:: Term -> Bool
redex (Var _) = False
redex (App (Abs _) _) = True
redex (App m n) = redex m || redex n
redex (Abs n) = redex n

-- De Bruijn form
indexOf:: (Eq a, Show a) => a -> [a] -> Maybe Int
indexOf e = go where 
    go [] = Nothing
    go (x:xs)
        | x == e = Just 0
        | otherwise = (+ 1) <$> go xs

rewrite:: [String] -> Term' -> Maybe Term
rewrite ctx = go where
    go (Var' x)   = Var <$> indexOf x ctx
    go (App' m n) = App <$> go m <*> go n
    go (Abs' (Var' x) n) = Abs <$> rewrite (x:ctx) n
    go _ = error "cannot rewrite such term"

shift:: Int -> Int -> Term -> Term
shift d = go where
    go c v@(Var i)
        | i >= c = Var $ i + d
        | otherwise = v
    go c (App m n) = App m' n' where
        m' = go c m
        n' = go c n
    go c (Abs n) = Abs n' where
        n' = go (c + 1) n

-- t[t'/x] t->x->t'->t[t'/x]
sub:: Term -> Int -> Term -> Term
sub (Var i) x t' 
    | x == i = t'
    | otherwise = Var i
sub (App m n) x t' = App m' n' where
    m' = sub m x t'
    n' = sub n x t'
sub (Abs n) x t' = Abs n' where
    n' = sub n (x + 1) $ shift 0 1 t'

step:: Term -> Term 
step (Var i) = Var i
step (Abs n) = Abs $ step n
step (App (Abs l) n) = shift 0 (-1) $ sub l 0 n
step (App m n)
    | redex m = App (step m) n
    | otherwise = App m $ step n

run:: Term -> Term
run = until (not . redex) step
