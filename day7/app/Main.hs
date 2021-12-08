{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Parse (parseLine)
import Problem (problem1, problem2)
import qualified Problem.Entertainment as Entertainment (problem1, problem2)

main :: IO ()
main = do
  (input :: [String]) <- lines <$> readFile "data/input"
  -- this time only the first line has data
  let parsedInput = parseLine $ head input

  let solution1 = problem1 parsedInput
  putStrLn $ "Day 7, Problem 1, direct, double traverse: " <> show solution1
  let solution2 = problem2 parsedInput
  putStrLn $ "Day 7, Problem 2: " <> show solution2

  let solution1 = Entertainment.problem1 parsedInput
  putStrLn $ "Day 7, Problem 1, direct, double traverse: " <> show solution1
  let solution2 = Entertainment.problem2 parsedInput
  putStrLn $ "Day 7, Problem 2: " <> show solution2