import Data.List

type Set a = [a]

singleton:: a -> Set a
singleton v = [v]

remove:: (Eq a) => a -> Set a -> Set a
remove a [] = []
remove a l
    | elem a l = delete a l
    | otherwise = l

data Term =
    Var String      -- x
    | Abs Term Term -- λx. M
    | App Term Term -- M N
    deriving (Eq)

type Conf = (Term, Int)

fv:: Term -> (Set Term)
fv t = case t of
    Var x -> singleton $ Var x
    Abs m n -> delete m $ fv n
    App m n -> union (fv m) (fv n)

-- t[t'/x]
sub:: Term -> Term -> Conf -> Conf
sub x t' c@(v@(Var _), i)
    | x == v = (t', i)
    | otherwise = c

sub x t' (App m n, i) =
    (App m' n', i'') where
        (m', i') = sub x t' (m, i)
        (n', i'') = sub x t' (n, i')

sub x t' (Abs v@(Var _) n, i)
    | x == v = (Abs v n, i)
    | elem v (fv t') = 
        let i' = i + 1
            (t'', i'') = sub v n (Var (show i), i') 
            (t''', i''') = sub x t' (t'', i'') in
                (Abs (Var (show i)) t''', i''')
    | otherwise = (Abs v n', i') where 
        (n', i') = sub x t' (n, i)

redex:: Term -> Bool
redex (Var _) = False
redex (App _ _) = True
redex (Abs m n) = (redex m) || (redex n)

step:: Conf -> Conf
step (App m n, i) = case m of
    Abs x l -> sub x n (l, i)
    otherwise -> (App m' n, i') where (m', i') = step (m, i)

step (Abs m n, i)
    | redex m = let (m', i') = step (m, i) in
        (Abs m' n, i') 
    | redex n = let (n', i') = step (n, i) in
        (Abs m n', i')
    | otherwise = (Abs m n, i)
step x = x

instance Show Term where
    show t = case t of
        (Var x) -> x
        (App m n) -> "(" ++ (show m) ++ ") (" ++ (show n) ++ ")"
        (Abs m n) -> "λ" ++ (show m) ++ "." ++ (show n)

prot = 
    (App 
        (Abs (Var "x") (Var "x")) 
        (App 
            (Abs (Var "x") (Var "x")) 
            (Abs (Var "z") 
                (App 
                    (Abs (Var "x") (Var "x")) 
                    (Var "z")))))