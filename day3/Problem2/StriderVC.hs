{-# LANGUAGE ScopedTypeVariables #-}
module Problem2.StriderVC (problem2) where

import Common (parseBit, evaluate)
import Data.List (transpose, maximumBy, minimumBy, group, sort)
import Data.Ord (comparing)
-- inspired by
-- https://github.com/stridervc/aoc2021/blob/main/src/Day03.hs


type Byte = [Int] -- shall only cotain 1s and 0s

rating :: CriterionSelector -> Int -> [Byte] -> Byte
rating c _ [s]  = s
rating c i ss   = rating c (i+1) $ filter (\s -> s!!i == criterion) ss
  where bits    = map (!!i) ss
        zeros   = length $ filter (==0) bits
        ones    = length $ filter (==1) bits
        criterion | zeros `c` ones  = 0
                  | otherwise     = 1

type CriterionSelector = Int -> Int -> Bool

-- there's an assymetry there:
mostOrEqual :: CriterionSelector
mostOrEqual = (>)
leastOrEqual :: CriterionSelector
leastOrEqual = (<=)
-- `criterion` would be, not generalized, the following:
--      most    | zeros > ones  = '0'
--              | otherwise     = '1'
--      least   | ones < zeros  = '1'
--              | otherwise     = '0'

problem2 :: [String] -> (Int, Int) -- oxygen, co2
problem2 input =
    let bytes :: [Byte] = fmap parseBit <$> input
    in ( evaluate $ rating mostOrEqual 0 bytes,
         evaluate $ rating leastOrEqual 0 bytes)
