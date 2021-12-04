module Problem (problem1, problem2) where

import Common
import Data.List (transpose)

problem1 :: [Int] -> [Board] -> (Board, Int)
problem1 [] _ = error "no winner!"
problem1 (r : rs) bs = case winningBoard of
    [] -> problem1 rs newBs
    wb : _ -> (wb, r)
  where
    newBs = applyDeep markHit bs
    applyDeep = fmap . fmap . fmap
    winningBoard = filter boardWins newBs
    markHit x
        | x == r    = -1
        | otherwise = x

boardWins :: Board -> Bool
boardWins b = checkRow b || (checkRow . transpose) b
    where
        checkRow = any (all (<0))

problem2 :: [Int] -> [Board] -> (Board, Int)
problem2 [] _ = error "no winner!"
problem2 (r : rs) bs = case nonWonBs of
    [wb] -> problem1 rs nonWonBs
    wb : wbs -> problem2 rs nonWonBs
    _ -> error "should not happen!"
  where
    newBs = applyDeep markHit bs
    nonWonBs = filter (not . boardWins) newBs
    applyDeep = fmap . fmap . fmap
    markHit x
        | x == r    = -1
        | otherwise = x

