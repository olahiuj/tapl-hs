module Main where

import System.IO
import Grammar 
import Term
import Type
import Exec

parseFile :: String -> IO Term
parseFile file = do
  parse <$> readFile file

main :: IO ()
main = do
  file <- do
    _ <- putStr "relative filepath: "
    _ <- hFlush stdout
    getLine
  term <- parseFile file
  print $ typecheck term
  loop term

loop :: Term -> IO ()
loop term = do
  index <- read <$> do
    _ <- putStr ":> "
    _ <- hFlush stdout
    getLine
  let states = term:map reduce states
  print $ states !! index
  loop term
