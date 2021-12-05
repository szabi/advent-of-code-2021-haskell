{-# LANGUAGE TupleSections #-}
module Problem.JHidding (problem1, problem2) where

import Common
import Data.List (transpose, partition)
import Data.Tuple (swap)

problem1 = solutionA
problem2 = solutionB

-- this one is a translation of
-- <https://github.com/jhidding/aoc2021/blob/main/lit/day04.md>
--
-- The original is incredibly easy to read. I hope that clarity does
-- not get lost in translation.
--
-- some of the clarity *is* due to using porper types. As with Chinn,
-- that will get lost somewhat by stripping it down and moulding it
-- to my infrastructure.
--
-- This is a solution I find remarkable.


-- For part A we need to figure out, the first board to win and the last
-- number that was called. I won't pretend this is the first implementation
-- I came up with. After also solving part B, it turns out this is the most
-- elegant and generic way to do it. The function winners generates a list
-- of (Int, Board) pairs, giving in order each board winning and on what number:
winSeq :: [Int] -> [Board] -> [Output]
winSeq []       _       = []
winSeq _        []      = []
winSeq (d:draws) boards = map (,d) winners <> winSeq draws losers
    where (winners, losers) = partition winning $ updateWith d <$> boards

-- Solution

solutionA :: [Int] -> [Board] -> Output
solutionA draws boards = head (winSeq draws boards)

solutionB:: [Int] -> [Board] -> Output
solutionB draws boards = last (winSeq draws boards)

