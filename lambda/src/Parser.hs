module Parser where
import Common

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