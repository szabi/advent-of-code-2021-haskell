module Problem.Cavalieri (problem1, problem2) where

import Common
import Data.List (transpose)

problem1 = firstWinningBoard
problem2 = lastWinningBoard

-- this one is a translation of
-- <https://github.com/giacomocavalieri/aoc-2021/blob/main/src/Days/Day4.hs>
--
-- In spririt, this is similar to my solution, but the way he abstracted
-- out a common core is impressive. It looks pleasing, though the output
-- type of `drawUntilOneWins` is quite a monster.
--
-- Is this kind of generalization worth it? Does it increase readability?


-- Keep drawing numbers until one (or more) winning board is found, returns:
-- * the last extracted number
-- * the numbers that were not extracted
-- * the first of all the winning boards
-- * the remaining boards updated to reflect the extractions of the numbers
drawUntilOneWins :: [Int] -> [Board] -> (Int, [Int], Board, [Board])
drawUntilOneWins (n:ns) boards = case winningBoards of
    (board:_) -> (n, ns, board, losingBoards)
    []        -> drawUntilOneWins ns boards'
    where boards'       = updateWith n <$> boards
          winningBoards = filter winning boards'
          losingBoards  = filter (not . flip elem winningBoards) boards'

firstWinningBoard :: [Int] -> [Board] -> Output
firstWinningBoard ns boards = (board, n)
    where (n, _, board, _) = drawUntilOneWins ns boards

lastWinningBoard :: [Int] -> [Board] -> Output
lastWinningBoard ns boards
    | null boards' = (winningBoard, winningNumber)
    | otherwise    = lastWinningBoard ns' boards'
    where (winningNumber, ns', winningBoard, boards') = drawUntilOneWins ns boards
