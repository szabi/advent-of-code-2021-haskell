{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Problem2.Pwm (problem2) where

import Common (parseBit, evaluate, invert)
import Data.List (transpose, maximumBy, minimumBy, group, sort, partition)
import Data.Ord (comparing)
import Control.Monad (join)
import Data.Bifunctor (bimap)

-- inspired by
-- https://github.com/pwm/aoc2021/blob/master/src/AoC/Days/Day03.hs


type Bit = Int -- shall only cotain 1s and 0s
type Byte = [Bit]

mcb, lcb :: Byte -> Bit
mcb l = fromEnum $ length (filter (== 1) l) >= ceiling (fromIntegral @Int @Float (length l) / 2)
lcb = invert . mcb

bitCriteria :: (Byte -> Bit) -> Int -> [Byte] -> [Byte]
bitCriteria crit pos bbs =
  filter (\bs -> bs !! pos == crit (transpose bbs !! pos)) bbs

findWith :: (Int -> [Byte] -> [Byte]) -> [Byte] -> Byte
findWith finder = go 0
  where
    go :: Int -> [Byte] -> Byte
    go pos bbs
      | length bbs == 1 = head bbs
      | otherwise = go (pos + 1) (finder pos bbs)

problem2 :: [String] -> (Int, Int) -- oxygen, co2
problem2 input =
    let bytes :: [Byte] = fmap parseBit <$> input
        wires = transpose bytes
        o2  = evaluate $ findWith (bitCriteria mcb) bytes
        co2 = evaluate $ findWith (bitCriteria lcb) bytes
    in ( o2, co2 )
