{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Problem1.Overengineered as Overengineered (problem1)
import Problem1 (problem1)
import qualified Problem2.Topaz as Topaz (problem2)
import qualified Problem2.StriderVC as StriderVC (problem2)
import qualified Problem2.Cavalieri as Cavalieri (problem2)
import qualified Problem2.Pwm as Pwm (problem2)

main :: IO ()
main = do
        (input :: [String]) <- lines <$> readFile "input.txt"

        let (gamma, epsilon) = Overengineered.problem1 input
        putStrLn $ tell 1 ", overengineered" gamma epsilon

        let (gamma, epsilon) = problem1 input
        putStrLn $ tell 1 ",         direct" gamma epsilon

        putStrLn ""

        let (oxy, co2) = Topaz.problem2 input
        putStrLn $ tell 2 ",          topaz" oxy co2

        let (oxy, co2) = StriderVC.problem2 input
        putStrLn $ tell 2 ",      stridervc" oxy co2

        let (oxy, co2) = Cavalieri.problem2 input
        putStrLn $ tell 2 ",      cavalieri" oxy co2

        let (oxy, co2) = Pwm.problem2 input
        putStrLn $ tell 2 ",            pwm" oxy co2


tell :: Int -> String -> Int -> Int -> String
tell problem comment gamma epsilon =  "Day 3, Problem " <> show problem <> comment <> ": γ=" <> show gamma <> ", ε=" <> show epsilon <> ", solution = " <> show (gamma * epsilon)
