module Problem1 (problem1) where

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