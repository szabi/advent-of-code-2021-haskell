{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Problem (problem1, problem2) where

import Common
import Data.List.Extra ((!?))
import Data.Maybe (fromMaybe, catMaybes)


-- we do assume square input
problem1 :: Input -> Output
problem1 xss = let ly = length xss
                   lx = length $ head xss
                in sum $ danger <$> catMaybes [lowest xss x y | x <- [0..lx-1], y <- [0..ly-1]]

danger :: Height -> Int
danger = (+1)

problem2 = undefined



-- semi-safe: x,y must be within bounds,
-- but it can check for neighbours outside
lowest :: Input -> Int -> Int -> Maybe Height
lowest i x y = if minPoint then Just val else Nothing
    where
        val = i !! y !! x
        minPoint = val < left && val < right && val < top && val < bottom
        left   = i !!  y !? (x - 1) ?: maxBound
        right  = i !!  y !? (x + 1) ?: maxBound
        top    = i !? (y - 1) ?: repeat maxBound !! x
        bottom = i !? (y + 1) ?: repeat maxBound !! x

-- the same name is used in "relude" and "errors" package.
-- in GHC.Data.Maybe it's called `orElse`
(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe