{-# LANGUAGE ScopedTypeVariables #-}
module Problem2.Cavalieri (problem2) where

import Common (parseBit, evaluate)
import Data.List (transpose, maximumBy, minimumBy, group, sort, partition)
import Data.Ord (comparing)
import Control.Monad (join)
import Data.Bifunctor (bimap)
-- inspired by
-- https://github.com/giacomocavalieri/aoc-2021/blob/main/src/Days/Day3.hs


type Byte = [Int] -- shall only cotain 1s and 0s

sieve :: Int -> (Int -> Int -> Bool) -> [Byte] -> Byte
sieve _ _ [b] = b
sieve pos predicate bs = sieve (pos + 1) predicate $ filter ((predicate common) . (!! pos)) bs
    where common = mostCommon $ map (!! pos) bs


mostCommon :: Byte -> Int
mostCommon bs = if ones >= zeros then 1 else 0
    where (ones, zeros)  = join bimap length $ partition (== 1) bs

problem2 :: [String] -> (Int, Int) -- oxygen, co2
problem2 input =
    let bytes :: [Byte] = fmap parseBit <$> input
        wires = transpose bytes
        o2  = evaluate $ sieve 0 (==) bytes
        co2 = evaluate $ sieve 0 (/=) bytes
    in ( o2, co2 )
