module Common (
    Board,
    Output,
    winning,
    updateWith
) where
import Data.List (transpose)

type Board = [[Int]]

type Output = (Board, Int)


winning :: Board -> Bool
winning board = any completeRow board ||
                any completeRow flippedBoard
    where completeRow = all (<0)
          flippedBoard = transpose board


updateWith :: Int -> Board -> Board
updateWith n = (map . map) markDrawn
    where markDrawn x | x == n    = -1
                      | otherwise =  x
