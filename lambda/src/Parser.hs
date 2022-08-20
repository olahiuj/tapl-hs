module Parser 
    ( Parser, Err
    , pChar, pString, pWS, pCharCI, pStringCI
    , pId, pInt, genParser
    , anyOf, some, many, optional, sepBy
    , between, pPar, pBrac, pCurBrac
    , runParser) where

import Data.Char (toLower, toUpper)

-- error
data Err c = Err [c] deriving (Eq);

instance (Show c) => Show (Err c) where
  show (Err xs) = "Expect " ++ show xs

instance Semigroup (Err c) where
  Err c1 <> Err c2 = Err $ c1 ++ c2

instance Monoid (Err c) where
  mempty = Err []

-- parser
type Result c a = (a, [c])

newtype Parser c a = Parser {
    runParser :: [c] -> Either (Err c) (Result c a)
}

satisfy :: (Eq c, Show c) => c -> Parser c c
satisfy expect = Parser go where
    go [] = Left $ Err [expect]
    go (x:xs)
        | (== expect) x = Right (x, xs)
        | otherwise = Left $ Err [expect]

-- transform
instance Functor (Parser c) where
    fmap f (Parser p) = Parser $ \s -> do
        (a, c) <- p s
        return (f a, c)

-- orElse
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative (Parser c) where
    empty = Parser $ const $ Left mempty

    Parser p1 <|> Parser p2 = Parser $ \s ->
        case p1 s of 
            Right (a1, c1) -> Right (a1, c1)
            Left err1 -> case p2 s of 
                Right (a2, c2) -> Right (a2, c2)
                Left err2 -> Left $ err1 <> err2

-- andThen
instance Applicative (Parser c) where
    pure x = Parser $ \s -> Right (x, s)
    
    Parser f <*> Parser p = Parser $ \s -> do
        (a1, c1) <- f s
        (a2, c2) <- p c1
        return (a1 a2, c2)

instance Monad (Parser c) where
    return = pure
    
    Parser p >>= f = Parser $ \s -> do
        (a, c) <- p s
        runParser (f a) c

-- combinators
anyOf :: [Parser c a] -> Parser c a
anyOf = foldr (<|>) empty

some :: Parser c a -> Parser c [a]
some p = do
    c1 <- p
    c2 <- many p
    return (c1:c2)

many :: Parser c a -> Parser c [a]
many (Parser p) = Parser go where
    go s = either fLeft fRight $ p s where
        fLeft = const $ Right ([], s)
        fRight (a1, c1) = Right (a1:a2, c2) where
            Right (a2, c2) = go c1

between :: Parser c a1 -> Parser c a2 -> Parser c a3 -> Parser c a3
between l r m = l *> m <* r

sepBy :: Parser c a -> Parser c a -> Parser c [a]
sepBy s p = do
    x  <- p
    xs <- many $ do
        _ <- s
        r <- p
        return r
    return (x:xs)

-- basics
genParser :: (Show a, Read a) => a -> Parser Char a
genParser = (read <$>) . pString . show

pChar :: Char -> Parser Char Char
pChar = satisfy

pString :: String -> Parser Char String
pString = sequence . fmap pChar

pLetter :: Parser Char Char
pLetter = anyOf $ pChar <$> ['a' .. 'z']

pId :: Parser Char String
pId = some pLetter

pDigit :: Parser Char Char
pDigit = anyOf $ pChar <$> ['0' .. '9']

pNonZeroDigit :: Parser Char Char
pNonZeroDigit = anyOf $ pChar <$> ['1' .. '9']

pInt :: Parser Char Int
pInt = do
    x  <- pNonZeroDigit
    xs <- many pDigit
    return $ read (x:xs)

pPar :: Parser Char a -> Parser Char a
pPar = between (pChar '(') (pChar ')')

pBrac :: Parser Char a -> Parser Char a
pBrac = between (pChar '[') (pChar ']')

pCurBrac :: Parser Char a -> Parser Char a
pCurBrac = between (pChar '{') (pChar '}')

-- case insensitive, lowercase is returned
pChar':: Char -> Parser Char Char
pChar' c = toLower <$> ((pChar cLO) <|> (pChar cUP)) where
    cLO = toLower c
    cUP = toUpper c

pString':: String -> Parser Char String
pString' = traverse pCharCI

pWS:: Parser Char Char
pWS = anyOf $ pChar <$> ['\n','\t',' ']

optional:: (Parser c a) -> (Parser c (Maybe a))
optional (Parser p) = Parser $ \s ->
    let fLeft  _      = Right (Nothing, s)
        fRight (a, c) = Right (Just a, c)
        in either fLeft fRight $ p s
