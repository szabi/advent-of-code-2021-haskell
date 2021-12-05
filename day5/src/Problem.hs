{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
module Problem (problem1, problem2) where

import Common
import Data.Maybe ( mapMaybe)
import Data.List (group, sort)

data Line = Vertical   { x1 :: Int, x2 :: Int, y0 :: Int }
          | Horizontal { x0 :: Int, y1 :: Int, y2 :: Int }
          | RDiagonal   { x1 :: Int, y1 :: Int, x2 :: Int, y2 ::Int }
          | LDiagonal   { x1 :: Int, y1 :: Int, x2 :: Int, y2 ::Int }

-- This validates line:
-- it guarantees that for the heterogenous component a1 <= a2
-- This property allows us to use list comprehension in expand.
--
-- Usually, `Line` and `line` would be in a library and only
-- `line` exported as a smart constructor. We do not move it
-- to our module "Common" here, so that the code stays easily
-- pastable and self-containing for sharing.
line :: Coord -> Coord -> Line
line (Coord x1 y1) (Coord x2 y2)
    | x1 == x2  = Horizontal x1 (min y1 y2) (max y1 y2)
    | y1 == y2  = Vertical   (min x1 x2) (max x1 x2) y1
    -- for diagonal, there is no natural sorting in 2D space.
    -- however, we fortunately won't need this to expand our
    -- line
    | abs (x2 - x1) /= abs (y2 - y1) = error "invalid input!"
    | signum (x2 - x1) == signum (y2 - y1) = RDiagonal (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)
    -- convention: x is in order
    | otherwise                      = LDiagonal (min x1 x2) (max y1 y2) (max x1 x2) (min y1 y2)

isStraight :: Line -> Bool
isStraight = not . isDiagonal
isDiagonal :: Line -> Bool
isDiagonal (RDiagonal {} ) = True
isDiagonal (LDiagonal {} ) = True
isDiagonal _ = False

problem1 = problem isStraight
problem2 = problem all
  where all = const True

problem :: (Line -> Bool) -> Input -> Output
problem valid cps = let
    -- hydrothermal vent lines
    validVentLines :: [Line] =  filter valid $ uncurry line <$> cps
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
expand (Vertical   x1 x2 y0)   = [Coord x y0 | x <- [x1..x2] ]
expand (Horizontal x0 y1 y2)   = [Coord x0 y | y <- [y1..y2] ]
expand (RDiagonal x1 y1 x2 y2) = [Coord x  y | x <- [x1..x2] | y <- [y1..y2]] 
expand (LDiagonal x1 y1 x2 y2) = [Coord x  y | x <- [x1..x2] | y <- [y1,y1-1..y2]] 

