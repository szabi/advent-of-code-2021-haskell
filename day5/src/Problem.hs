{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}
module Problem (problem1, problem2) where

import Common
import Data.Maybe ( mapMaybe)
import Data.List (group, sort)

type Line = (Coord, Coord)

isStraight :: Line -> Bool
isStraight (Coord x1 y1, Coord x2 y2) = x1 == x2 || y1 == y2


problem1 = problem isStraight
problem2 = problem all
  where all = const True

problem :: (Line -> Bool) -> Input -> Output
problem valid cps = let
    -- hydrothermal vent lines
    validVentLines :: [Line] =  filter valid cps
    -- coordinates hit by all lines. With multiplicities

    coords = concat $ expand <$> validVentLines
    grouped = group $ sort coords
    dangerous = filter ((>1) . length) grouped
    -- We have finished it with filtering for length >1 here.
    -- we could have further processed the list to make it more
    -- robust for further changes and make it more readable, like
    -- using a further "collect" on the above dangerous list:
    --
    -- cf. <https://github.com/brandonchinn178/advent-of-code/blob/main/2021/Day05.hs>
    -- toHistogram :: Ord a => [a] -> [(a, Int)]
    --toHistogram = map collect . group . sort
    --  where
    --    collect xs@(x:_) = (x, length xs)
  in length dangerous

-- Compared to our original solution, we simplify the handling by following the
-- custome 'range' function (instead of builtin [a..b]), as inspired by
-- <https://old.reddit.com/r/haskell/comments/r982ip/advent_of_code_2021_day_05/hnaju7a/>

-- This implementation uses the property that only
-- 45° diagonal lines are allowed in the input by
-- specification!
expand :: Line -> [Coord]
expand (Coord x1 y1, Coord x2 y2)
  | x1 == x2 = map (uncurry Coord . (x1,)) (range y1 y2) -- horizontal line
  | y1 == y2 = map (uncurry Coord . (,y1)) (range x1 x2) -- vertical line
  | otherwise = zipWith Coord (range x1 x2) (range y1 y2)


range :: Int -> Int -> [Int]
range a b
  | a < b = [a..b]
  | otherwise = [a,(a-1)..b]


-- NOTE:
-- another approach to study is <https://github.com/sullyj3/adventOfCode2021/blob/main/src/Day05.hs>
-- with its monoid 'PointCounter'.