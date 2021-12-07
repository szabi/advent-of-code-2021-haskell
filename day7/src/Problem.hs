module Problem (problem1, problem2) where

import Common

problem1 = problem id
problem2 = problem increasingCost

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

type Distance = Int
type Cost = Int
type FuelCostFn = Distance -> Cost

problem :: FuelCostFn -> Input -> Output
problem fuelCost xs = let
                  minx = minimum xs
                  maxx = maximum xs
                  -- ok, there is something nice to be done with `Arrow` here!
                  minval = fst $ bisector fuelCost (minx, maxx) xs
                  maxval = snd $ bisector fuelCost (minx, maxx) xs
                  absDiff1 = absDiff fuelCost minval xs
                  absDiff2 = absDiff fuelCost maxval xs
              in min absDiff1 absDiff2

bisector :: FuelCostFn -> (Int, Int) -> Input -> (Int, Int)
bisector fuelCost (min, max) xs
  | min == max     = (min, min)
  | min == max - 1 = (min, max)
  | otherwise  = let
      minx = absDiff fuelCost min xs
      maxx = absDiff fuelCost max xs
      mid = (min + max) `div` 2
      midx = absDiff fuelCost mid xs
      newPair = if minx + midx < maxx + midx then (min, mid) else (mid, max)
      in bisector fuelCost newPair xs

absDiff :: FuelCostFn -> Distance -> Input -> Cost
absDiff fuelCost m xs = sum $ map (fuelCost . abs . (m -)) xs

distances :: [Distance]
distances = [1..]

costs :: [Cost]
costs = scanl (+) 0 distances

increasingCost :: FuelCostFn
increasingCost x = costs !! x