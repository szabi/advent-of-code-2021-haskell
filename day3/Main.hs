{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Problem1.Overengineered as Overengineered (problem1)
import Problem1 (problem1)

main :: IO ()
main = do
        (input :: [String]) <- lines <$> readFile "input.txt"

        let (gamma, epsilon) = Overengineered.problem1 input
        putStrLn $ tell 1 ", overengineered" gamma epsilon

        let (gamma, epsilon) = problem1 input
        putStrLn $ tell 1 ",         direct" gamma epsilon


tell :: Int -> String -> Int -> Int -> String
tell problem comment gamma epsilon =  "Day 3, Problem " <> show problem <> comment <> ": γ=" <> show gamma <> ", ε=" <> show epsilon <> ", solution = " <> show (gamma * epsilon)
