{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Problem (problem1, problem2)

main :: IO ()
main = do
  (input :: [String]) <- lines <$> readFile "data/input"

  let solution1 = problem1 input
  putStrLn $ "Day 10, Problem 1: " <> show solution1
  let solution2 = problem2 input
  putStrLn $ "Day 10, Problem 2: " <> show solution2
