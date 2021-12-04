{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Common (Board)
import Problem (problem1, problem2)

-- This `split` is generalized from `GHC.Utils.Misc`
-- GHC.Utils.Misc.split is typed  :: Char -> String -> [String]
-- 
-- this is reall something we could have in `base`
split :: (Eq a) => a -> [a] -> [[a]]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

-- Boards can -- unfortunately -- contain "0", however they cannot contain
-- negative numbers. We'll use the convention that "-1" means "hit"
-- this can turn out to be a problem if "problem 2" is connected and differently
-- engineered. Maybe we should go straight for [[Maybe Int]]?
evaluate :: Board -> Int
evaluate board = sum $ sum <$> (fmap . fmap) (\x -> if x < 0 then 0 else x) board


main :: IO ()
main = do
        -- preprocessing
        (input :: [String]) <- lines <$> readFile "input.txt"
        let (random : boards) = split "" input
        -- parsing input data
        let b :: [Board] = (fmap . fmap . fmap) (read @Int) $ (fmap . fmap) (filter (/="") . split ' ') boards
        let r :: [Int] = read @Int <$> split ',' (head random)
        -- calculation
        let (winningBoard , lastrandom) = Problem.problem1 r b
        putStrLn $ tell 1 "" (evaluate winningBoard) lastrandom

        let (loosingBoard , itsWinning) = Problem.problem2 r b
        putStrLn $ tell 2 "" (evaluate loosingBoard) itsWinning


tell :: Int -> String -> Int -> Int -> String
tell problem comment boardSum lastRandom =  "Day 4, Problem " <> show problem <> comment <> ": boardSum=" <> show boardSum <> ", lastRandom=" <> show lastRandom <> ", solution = " <> show (boardSum * lastRandom)
