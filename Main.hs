module Main where

import TermGame

main :: IO ()
main = do
  putStrLn "Welcome to mines!"
  playGame
  putStrLn "Game over"

