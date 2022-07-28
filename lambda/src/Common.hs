module Common where

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
    show (Abs n) = "(λ." ++ (show n) ++ ")"
    show (App m n) = case m of
        (Var x) -> (show x) ++ (show n)
        otherwise -> "(" ++ (show m) ++ ") " ++ (show n)

instance Show Term' where
    show (Var' x) = x
    show (Abs' m n) = "(λ" ++ (show m) ++ "." ++ (show n) ++ ")"
    show (App' m n) = "(" ++ (show m) ++ " " ++ (show n) ++ ")"

prot = 
    (App' 
        (Abs' (Var' "x") (Var' "x")) 
        (App' 
            (Abs' (Var' "x") (Var' "x")) 
            (Abs' (Var' "z") 
                (App' 
                    (Abs' (Var' "x") (Var' "x")) 
                    (Var' "z")))))

tru = 
    (Abs'
        (Var' "t")
        (Abs'
            (Var' "f")
            (Var' "t")))

fls = 
    (Abs'
        (Var' "t")
        (Abs'
            (Var' "f")
            (Var' "f")))

band = 
    (Abs'
        (Var' "a")
        (Abs'
            (Var' "b")
            (App'
                (App'
                    (Var' "a")
                    (Var' "b"))
                fls)))

zero = fls

suc =
    (Abs' 
        (Var' "n")
        (Abs'
            (Var' "f")
           (Abs'
                (Var' "x")
                (App'
                    (Var' "f")
                    (App'
                        (App'
                            (Var' "n")
                            (Var' "f"))
                        (Var' "x"))))))
-- f(((λt.λf.f)f)x)

one = (App' suc zero)
two = (App' suc one)
thr = (App' suc two)

divergent =
    (Abs'
        (Var' "x")
        (App'
            (App'
                (Var' "x")
                (Var' "x"))
            (App'
                (Var' "x")
                (Var' "x"))))

comb_Y = 
    (Abs'
        (Var' "F")
        (App'
            (Abs'
                (Var' "x")
                (App'
                    (Var' "F")
                    (App'
                        (Var' "x")
                        (Var' "x"))))
            (Abs'
                (Var' "x")
                (App'
                    (Var' "F")
                    (App'
                        (Var' "x")
                        (Var' "x"))))))

if_then_else = 
    (Abs'
        (Var' "e")
        (Abs'
            (Var' "a")
            (Abs'
                (Var' "b")
                (App'
                    (App'
                        (Var' "e")
                        (Var' "a"))
                    (Var' "b")))))

{-
prot
prot_ = getTerm $ runParser pTerm $ show prot
prot_ == prot
nprot = rewrite [] prot_
run nprot
-}