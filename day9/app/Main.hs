{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Parse (parseLine)
import Problem (problem1, problem2)
import qualified Problem.DemiAoC as DemiAoC (problem1, problem2)

main :: IO ()
main = do
  (input :: [String]) <- lines <$> readFile "data/input"
  -- this time only the first line has data
  let parsedInput = parseLine <$> input
  -- let finalInput = preprocess parsedInput
  let ly = length parsedInput
  let lx = length $ head parsedInput
  print (lx, ly)

  let solution1 = problem1 parsedInput
  putStrLn $ "Day 8, Problem 1: " <> show solution1


  let solution1 = DemiAoC.problem1 parsedInput
  putStrLn $ "Day 8, Problem 1, demiAoC: " <> show solution1
  let solution2 = DemiAoC.problem2 parsedInput
  putStrLn $ "Day 8, Problem 2, demiAoC: " <> show solution2
