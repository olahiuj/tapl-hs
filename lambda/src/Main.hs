import Data.List
import Data.Char

-- defs
type Set a = [a]

singleton:: a -> Set a
singleton v = [v]

remove:: (Eq a) => a -> Set a -> Set a
remove a [] = []
remove a l
    | elem a l = delete a l
    | otherwise = l

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

redex:: Term -> Bool
redex (Var _) = False
redex (App (Abs _) _) = True
redex (App m n) = (redex m) || (redex n)
redex (Abs n) = redex n

-- parser
data Parser c a
    = Success {
        getTerm:: a,
        inputStream:: [c]
    }
    | Failure String
    | Parser {
        runParser:: [c] -> (Parser c a)
    }

instance (Show a, Show c) => Show (Parser c a) where
    show (Success a c) = show (a, c)
    show (Failure str) = show "err: " ++ str
    show (Parser p) = "Parser"

satisfy:: (Eq c, Show c) => (c -> Bool) -> Parser c [c]
satisfy predicate = Parser go where
    go [] = Failure "EOF"
    go (x:xs)
        | predicate x = Success [x] xs
        | otherwise = Failure $ "Only to find " ++ (show x)

pChar:: Char -> Parser Char [Char]
pChar c = satisfy (== c)

-- transform
instance Functor (Parser c) where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Failure err -> Failure err
            Success result rest -> Success (f result)  rest

-- orElse
class (Applicative f) => Alternative f where
    empty:: f a
    (<|>):: f a -> f a -> f a

instance Alternative (Parser c) where
    empty = Parser $ \input -> Failure "empty"

    Parser p1 <|> Parser p2 = Parser $ \input ->
        case p1 input of 
            Success result1 rest1 -> Success result1 rest1
            Failure err1 -> case p2 input of 
                Success result2 rest2 -> Success result2 rest2
                Failure err2 -> Failure $ err1

-- andThen
instance Applicative (Parser c) where
    pure x = Parser $ \input -> Success x input
    
    Parser f <*> Parser p = Parser $ \input ->
        case f input of
            Failure err1 -> Failure err1
            Success result1 rest1 -> case p rest1 of
                Failure err2 -> Failure err2
                Success result2 rest2 -> Success (result1 result2) rest2

instance Monad (Parser c) where
    return = pure
    
    Parser p >>= f = Parser $ \input ->
        case p input of
            Failure err -> Failure err
            Success result rest -> 
                p' rest where
                    Parser p' = f result

anyOf:: [Parser c a] -> (Parser c a)
anyOf (x:[]) = x
anyOf (x:xs) = x <|> (anyOf xs)

many:: (Parser c a) -> (Parser c [a])
many (Parser p) = Parser $ go where
    go input = case p input of
        Failure err -> Success [] input
        Success result1 rest1 ->
            Success (result1:result2) rest2 where
                Success result2 rest2 = go rest1

-- parsing λ-terms
pID = anyOf $ pChar <$> ['a'..'z']

pVar = Var' <$> pID

pTerm = pVar <|> pAbs <|> pApp

pAbs = do
    pChar '('
    pChar 'λ'
    v <- pID
    pChar '.'
    t <- pTerm
    pChar ')'
    return $ Abs' (Var' v) t

pApp = do
    pChar '('
    m <- pTerm
    pChar ' '
    n <- pTerm
    pChar ')'
    return $ App' m n

-- De Bruijn form
indexOf:: (Eq a, Show a) => a -> [a] -> (Maybe Int)
indexOf e = go where 
    go [] = Nothing
    go (x:xs)
        | x == e = Just 0
        | otherwise = (+ 1) <$> go xs

rewrite:: [String] -> Term' -> Maybe Term
rewrite ctx = go where
    go (Var' x)   = Var <$> indexOf x ctx
    go (App' m n) = App <$> (go m) <*> (go n)
    go (Abs' (Var' x) n) = Abs <$> rewrite (x:ctx) n

shift:: Int -> Int -> Term -> Term
shift d = go where
    go c v@(Var i)
        | i >= c = (Var $ i + d)
        | otherwise = v
    go c (App m n) = (App m' n') where
        m' = go c m
        n' = go c n
    go c (Abs n) = (Abs n') where
        n' = go (c + 1) n

-- t[t'/x] t->x->t'->t[t'/x]
sub:: Term -> Int -> Term -> Term
sub (Var i) x t' 
    | x == i = t'
    | otherwise = (Var i) 
sub (App m n) x t' = (App m' n') where
    m' = sub m x t'
    n' = sub n x t'
sub (Abs n) x t' = (Abs n') where
    n' = sub n (x + 1) (shift 0 1 t')

step:: Term -> Term 
step (Var i) = (Var i)
step (Abs n) = (Abs $ step n)
step (App (Abs l) n) = shift 0 (-1) $ sub l 0 n
step (App m n)
    | redex m = (App (step m) n)
    | otherwise = (App m $ step n)

run:: Term -> Term
run = until (not . redex) step

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