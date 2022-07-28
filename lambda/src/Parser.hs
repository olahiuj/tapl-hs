module Parser where

import Data.Either
    
import Common

-- parser
data Parser c a = Parser {
    runParser:: [c] -> Either String (a, [c])
}

satisfy:: (Eq c, Show c) => c -> Parser c [c]
satisfy expect = Parser go where
    go [] = Left "EOF"
    go (x:xs)
        | (== expect) x = Right ([x], xs)
        | otherwise = Left $ "Expecting " ++ (show expect) ++ ", only to find " ++ (show x)

pChar:: Char -> Parser Char [Char]
pChar = satisfy

-- transform
instance Functor (Parser c) where
    fmap f (Parser p) = Parser $ \input -> do
        (result, rest) <- p input
        return ((f result), rest)

-- orElse
class (Applicative f) => Alternative f where
    empty:: f a
    (<|>):: f a -> f a -> f a

instance Alternative (Parser c) where
    empty = Parser $ \input -> Left "empty"

    Parser p1 <|> Parser p2 = Parser $ \input ->
        case p1 input of 
            Right (result1, rest1) -> Right (result1, rest1)
            Left err1 -> case p2 input of 
                Right (result2, rest2) -> Right (result2, rest2)
                Left err2 -> Left $ err1

-- andThen
instance Applicative (Parser c) where
    pure x = Parser $ \input -> Right (x, input)
    
    Parser f <*> Parser p = Parser $ \input -> do
        (result1, rest1) <- f input
        (result2, rest2) <- p rest1
        return ((result1 result2), rest2)

instance Monad (Parser c) where
    return = pure
    
    Parser p >>= f = Parser $ \input -> do
        (result, rest) <- p input
        runParser (f result) rest

anyOf:: [Parser c a] -> (Parser c a)
anyOf (x:[]) = x
anyOf (x:xs) = x <|> (anyOf xs)

many:: (Parser c a) -> (Parser c [a])
many (Parser p) = Parser $ go where
    go input = case p input of
        Left err -> Right ([], input)
        Right (result1, rest1) ->
            Right (result1:result2, rest2) where
                Right (result2, rest2) = go rest1

fromString:: String -> Term'
fromString = fromRight (Var' "∅") . (fst <$>) . runParser pTerm

-- parsing λ-terms
pID = anyOf $ pChar <$> ['a'..'z']
pVar = Var' <$> pID
pTerm = pVar <|> pAbs <|> pApp

pAbs = do
    pChar '('
    pChar 'λ'
    v <- pVar
    pChar '.'
    t <- pTerm
    pChar ')'
    return $ Abs' v t

pApp = do
    pChar '('
    m <- pTerm
    pChar ' '
    n <- pTerm
    pChar ')'
    return $ App' m n