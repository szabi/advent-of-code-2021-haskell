{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Parse (parseLine)
import Problem (problem1, problem2)
import qualified Problem.FramedWithSilence as FWSilence (problem1, problem2)

main :: IO ()
main = do
  (input :: [String]) <- lines <$> readFile "data/input"
  -- this time only the first line has data
  let parsedInput = parseLine <$> input

  let solution1 = problem1 parsedInput
  putStrLn $ "Day 8, Problem 1: " <> show solution1
  let solution2 = problem2 parsedInput
  putStrLn $ "Day 8, Problem 2: " <> show solution2

  let solution1' = FWSilence.problem1 parsedInput
  putStrLn $ "Day 8, Problem 1, FramedWithSilence: " <> show solution1'
  let solution2' = FWSilence.problem2 parsedInput
  putStrLn $ "Day 8, Problem 2, FramedWithSilence: " <> show solution2'
