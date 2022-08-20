module Main where

import Data.Map (empty)
import System.IO
import Def
import Grammar 
import Compile
import Eval

parseFile :: String -> IO FTerm
parseFile file = do
  parse <$> readFile file

main :: IO ()
main = do
  file <- do
    _ <- putStr "filename: "
    _ <- hFlush stdout
    getLine
  term <- parseFile $ "/home/jjppp/Code/Project/tapl-hs/full/res/tests/" ++ file
  print $ typecheck term
  print $ eval $ compile $ desugar empty term
{-
  loop term

loop :: FTerm -> IO ()
loop term = do
  index <- read <$> do
    _ <- putStr ":> "
    _ <- hFlush stdout
    getLine
  let states = term:map reduce states
  print $ states !! index
  loop term-}
