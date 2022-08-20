module Common where

import Parser

import Data.Either (fromRight)

-- defs
data Term =
    Var Int            -- x
    | Abs Term         -- λ N
    | App Term Term    -- M N
    deriving (Eq)

data Term' =
    Var' String        -- x
    | Abs' Term' Term' -- λx. N
    | App' Term' Term' -- M N
    deriving (Eq)

-- for display
instance Show Term where
    show (Var x) = show x 
    show (Abs n) = "(λ." ++ show n ++ ")"
    show (App m n) = case m of
        (Var x) -> show x ++ show n
        _ -> "(" ++ show m ++ ") " ++ show n

instance Show Term' where
    show (Var' x) = x
    show (Abs' m n) = "λ" ++ show m ++ "." ++ show n
    show (App' m (Var' x)) = case m of
        Abs' _ _ -> "(" ++ show m ++ ") " ++ x
        _        -> show m ++ " " ++ x
    show (App' m n) = case m of
        Abs' _ _ -> "(" ++ show m ++ ") (" ++ show n ++ ")"
        _        -> show m ++ " (" ++ show n ++ ")"

λ:: Term' -> Term' -> Term'
λ = Abs'

(|>):: Term' -> Term' -> Term'
m |> n = App' m n

infixr 0 ·
(·):: (a -> b) -> a -> b
(·) = ($)

-- parsing λ-terms
fromString:: String -> Term'
fromString = fromRight (Var' "∅") . (fst <$>) . runParser pTerm

pVar:: Parser Char Term'
pVar = Var' <$> pId

pTerm:: Parser Char Term'
pTerm = anyOf [pVar, pAbs, pApp]

pAbs:: Parser Char Term'
pAbs = pPar $ do
    _ <- pChar 'λ'
    v <- pVar
    _ <- pChar '.'
    t <- pTerm
    return $ Abs' v t

pApp:: Parser Char Term'
pApp = pPar $ do
    m <- pTerm
    _ <- pChar ' '
    n <- pTerm
    return $ App' m n

-- tests
iden:: Term'
iden = λ x x 
    where
        x = Var' "x"

prot:: Term'
prot = iden |> (iden |> (λ x · iden |> x))
    where 
        x = Var' "x"

tru:: Term'
tru = λ t $ λ f t
    where
        t = Var' "t"
        f = Var' "f"

fls:: Term'
fls = λ t $ λ f f
    where
        t = Var' "t"
        f = Var' "f"

band:: Term'
band = λ a · λ b · a |> b |> fls
    where
        a = Var' "a"
        b = Var' "b"

zero:: Term'
zero = fls

suc:: Term'
suc = λ n · λ f · λ x · f |> (n |> f |> x)
    where
        n = Var' "n"
        f = Var' "f"
        x = Var' "x"
-- f(((λt.λf.f)f)x)

one:: Term'
one = suc |> zero
two:: Term'
two = suc |> one
thr:: Term'
thr = suc |> two

divergent:: Term'
divergent = λ x · (x |> x) |> (x |> x)
    where
        x = Var' "x"

combY:: Term'
combY = λ f · (λ x · f |> (x |> x)) |> (λ x · f |> (x |> x))
    where
        f = Var' "f"
        x = Var' "x"

ifThenElse:: Term'
ifThenElse = λ e · λ a · λ b · e |> a |> b
    where
        e = Var' "e"
        a = Var' "a"
        b = Var' "b"

{-
prot
prot_ = getTerm $ runParser pTerm $ show prot
prot_ == prot
nprot = rewrite [] prot_
run nprot
-}
