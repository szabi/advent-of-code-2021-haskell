module Problem.Cavalieri (problem1, problem2) where

import Common
import Data.List (transpose)

-- this one is a translation of
-- <https://github.com/giacomocavalieri/aoc-2021/blob/main/src/Days/Day4.hs>
--
-- I am not particularly impressed by this one, see other examples, but I find
-- it interesting *how* Giacomo extracted the common logic out into a generic
-- function.
--
-- I don't think it improves readability, but it is an interesting technique
-- to study.

drawNumber :: Int -> Board -> Board
drawNumber n = map (map switchToDrawn)
    where switchToDrawn x | x == n    = -1
                          | otherwise =  x

isDrawn :: Int -> Bool
isDrawn = (<0)

isWinning :: Board -> Bool
isWinning board = any isRowComplete board || any isRowComplete (transpose board)
    where isRowComplete = all isDrawn

-- Keep drawing numbers until one (or more) winning board is found, returns:
-- * the last extracted number
-- * the numbers that were not extracted
-- * the first of all the winning boards
-- * the remaining boards updated to reflect the extractions of the numbers
drawUntilOneWins :: [Int] -> [Board] -> (Int, [Int], Board, [Board])
drawUntilOneWins (n:ns) boards = case winningBoards of
    (board:_) -> (n, ns, board, losingBoards)
    []        -> drawUntilOneWins ns boards'
    where boards'       = map (drawNumber n) boards
          winningBoards = filter isWinning boards'
          losingBoards  = filter (not . flip elem winningBoards) boards'

problem1 = firstWinningBoard
firstWinningBoard :: [Int] -> [Board] -> (Board, Int)
firstWinningBoard ns boards = (board, n)
    where (n, _, board, _) = drawUntilOneWins ns boards

problem2 = lastWinningBoard
lastWinningBoard :: [Int] -> [Board] -> (Board, Int)
lastWinningBoard ns boards
    | null boards' = (winningBoard, winningNumber)
    | otherwise    = lastWinningBoard ns' boards'
    where (winningNumber, ns', winningBoard, boards') = drawUntilOneWins ns boards
