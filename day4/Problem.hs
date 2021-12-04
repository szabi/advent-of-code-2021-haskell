module Problem (problem1, problem2) where

import Common
import Data.List (transpose)


boardWins :: Board -> Bool
boardWins b = checkRow b || (checkRow . transpose) b
    where
        checkRow = any (all (<0))


problem1 :: [Int] -> [Board] -> (Board, Int)
problem1 [] _ = error "no winner!"
problem1 (r : rs) bs = case winningBoard of
    [] -> problem1 rs newBs
    wb : _ -> (wb, r)
  where
    newBs = updateBoards r bs
    winningBoard = filter boardWins newBs

problem2 :: [Int] -> [Board] -> (Board, Int)
problem2 [] _ = error "no winner!"
problem2 (r : rs) bs = case nonWonBs of
    [wb] -> problem1 rs nonWonBs
    wb : wbs -> problem2 rs nonWonBs
    _ -> error "should not happen!"
  where
    newBs = updateBoards r bs
    nonWonBs = filter (not . boardWins) newBs


updateBoards :: Int -> [Board] -> [Board]
updateBoards r = applyDeep (markHit r)

markHit :: Int -> Int -> Int
markHit r x
  | x == r    = -1
  | otherwise = x

applyDeep :: (Int -> Int) -> [Board] -> [Board]
applyDeep = fmap . fmap . fmap
