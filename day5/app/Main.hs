{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Parse (parseLine)
import Problem (problem1)

main :: IO ()
main = do
  (input :: [String]) <- lines <$> readFile "data/input"
  let parsedInput = parseLine <$> input

  let solution1 = problem1 parsedInput
  putStrLn $ "Day 5, Problem 1: " <> show solution1
