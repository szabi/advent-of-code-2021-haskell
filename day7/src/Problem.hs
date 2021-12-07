{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
module Problem (problem1, problem2) where

import Common
import Data.List (group, groupBy, sort, sortOn)
import Debug.Trace


problem2 = undefined

-- Ok, I thought I was clever, and this was essentially
-- 
-- Sum abs(x - mean(x))
--
-- but that does not actually work out, even if I use 
-- `Rational` rather than integer division.
--
-- But this is not the case! Consider the list [9,13,20].
-- This has a mean of 14, and the abs. differences to 14 are:
--   5, 1, 6.
-- But with 13 the differences are [4,0,7], so one less in total!
-- The solution is *not* the absolute differences to the mean!
--
-- So, let's indeed bisect :-(

problem1 :: Input -> Output
problem1 xs = let minx = minimum xs
                  maxx = maximum xs
                  -- ok, there is something nice to be done with `Arrow` here!
                  minval = fst $ bisector (minx, maxx) xs
                  maxval = snd $ bisector (minx, maxx) xs
                  absDiff1 = absDiff minval xs
                  absDiff2 = absDiff maxval xs
              in min absDiff1 absDiff2

bisector :: (Int, Int) -> [Int] -> (Int, Int)
bisector (min, max) xs
  | min == max     = (min, min)
  | min == max - 1 = (min, max)
  | otherwise  = let
      minx = absDiff min xs
      maxx = absDiff max xs
      mid = (min + max) `div` 2
      midx = absDiff mid xs
      newPair = if minx + midx < maxx + midx then (min, mid) else (mid, max)
      in trace (show (min, mid, max) <> show (minx, midx, maxx)) $ bisector newPair xs

absDiff :: Int -> [Int] -> Int
absDiff m xs = sum $ map (abs . (m -)) xs
