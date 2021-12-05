module Problem (problem1, problem2) where

import Common
import Data.List (transpose)



problem1 :: [Int] -> [Board] -> Output
problem1 [] _ = error "no winner!"
problem1 (r : rs) bs = case winningBoard of
    [] -> problem1 rs bs'
    wb : _ -> (wb, r)
  where
    bs' = updateWith r <$> bs
    winningBoard = filter winning bs'

problem2 :: [Int] -> [Board] -> Output
problem2 [] _ = error "no winner!"
problem2 (r : rs) bs = case nonWonBs of
    [wb] -> problem1 rs nonWonBs
    wb : wbs -> problem2 rs nonWonBs
    _ -> error "should not happen!"
  where
    bs' = updateWith r <$> bs
    nonWonBs = filter (not . winning) bs'
