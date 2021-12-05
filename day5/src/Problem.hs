{-# LANGUAGE ScopedTypeVariables #-}
module Problem (problem1) where

import Common
import Data.Maybe ( mapMaybe)
import Data.List (group, sort)

data Line = Vertical   { x1 :: Int, x2 :: Int, y0 :: Int }
          | Horizontal { x0 :: Int, y1 :: Int, y2 :: Int }

-- This validates line:
-- it guarantees that for the heterogenous component a1 <= a2
-- This property allows us to use list comprehension in expand.
--
-- Usually, `Line` and `line` would be in a library and only
-- `line` exported as a smart constructor. We do not move it
-- to our module "Common" here, so that the code stays easily
-- pastable and self-containing for sharing.
line :: Coord -> Coord -> Maybe Line
line (Coord x1 y1) (Coord x2 y2)
    | x1 == x2  = Just $ Horizontal x1 (min y1 y2) (max y1 y2)
    | y1 == y2  = Just $ Vertical   (min x1 x2) (max x1 x2) y1
    | otherwise = Nothing

problem1 :: Input -> Output
problem1 cps = let
    -- hydrothermal vent lines
    validVentLines :: [Line] = mapMaybe (uncurry line) cps
    -- coordinates hit by all lines. With multiplicities
    coords = concat $ expand <$> validVentLines
    grouped = group $ sort coords
    dangerous = filter ((>1) . length) grouped
  in length dangerous

-- This implementation uses the property that the two heterogenous
-- coordinates of the line are always in sorted order!
-- 
-- That is, it will only work reliably with 'Line's created with
-- the smart constructor 'line'.
expand :: Line -> [Coord]
expand (Vertical   x1 x2 y0) = [Coord x y0 | x <- [x1..x2] ]
expand (Horizontal x0 y1 y2) = [Coord x0 y | y <- [y1..y2] ]

