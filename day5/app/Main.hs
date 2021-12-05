{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Parse (parseLine)
import Problem (problem1, problem2)

main :: IO ()
main = do
  (input :: [String]) <- lines <$> readFile "data/input"
  let parsedInput = parseLine <$> input

  let solution1 = problem1 parsedInput
  putStrLn $ "Day 5, Problem 1: " <> show solution1
  let solution1 = problem2 parsedInput
  putStrLn $ "Day 5, Problem 2: " <> show solution1
