{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
module Problem (problem1, problem2) where

import Common

problem1 = problem 80 -- we want to get the result after 80 days
problem2 = undefined

type Days = Int

problem :: Days -> Input -> Output
problem i fs = length $ go i fs
  where go :: Int -> Input -> Input
        go 0 fs = fs -- fs stands for "fish (pl.)"
        go i fs | i < 0 = error $ "We can only estimate the number of fish for " <>
                                  "future days, not back into the past!"
                | otherwise = go (i-1) $ concatMap ageFish fs
        ageFish :: Int -> [Int]
        ageFish 0 = [6,8]
        ageFish x | x < 0 = error "This should not have happened. You screwed up."
                  | otherwise = [x - 1]
