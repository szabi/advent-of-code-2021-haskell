{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Problem.Chinn (problem1, problem2) where

import Common
import Data.Ord (Down(Down), comparing)
import Data.Maybe (mapMaybe)
import Data.List (maximumBy, transpose)
import Data.Foldable (foldlM)

-- this one is a translation of
-- <https://github.com/brandonchinn178/advent-of-code/blob/main/2021/Day04.hs>
--
-- The use of `Down` is interesting and to study. Compare to Topaz and Cavalieri

data RoundInfo = RoundInfo
  { board :: Board
  , roundNum :: Int
  , calledNum :: Int
  }

extract :: RoundInfo -> Output
extract RoundInfo{..} = (board, calledNum)

problem1 :: [Int] -> [Board] -> Output
problem1 n bs = extract $ getWinningRoundInfoBy (Down . roundNum) n bs
problem2 :: [Int] -> [Board] -> Output
problem2 n bs = extract $ getWinningRoundInfoBy roundNum n bs

getWinningRoundInfoBy :: Ord a => (RoundInfo -> a) -> [Int] -> [Board] -> RoundInfo
getWinningRoundInfoBy f nums boards =
  case mapMaybe (roundsToWin nums) boards of
    [] -> error "No boards won"
    allWonRoundsInfo -> maximumBy (comparing f) allWonRoundsInfo



-- For the given board and list of called numbers, return the
-- round info of the round when the Board won.
roundsToWin :: [Int] -> Board -> Maybe RoundInfo
roundsToWin nums boardVals =
  either Just (const Nothing) $
    foldlM go initialRoundInfo calledNumAndRoundNums
  where
    initialRoundInfo =
      RoundInfo
        { board = boardVals
        , roundNum = 0
        , calledNum = 0
        }

    -- called numbers annotated with the round number
    calledNumAndRoundNums = zip nums [1..]

    go :: RoundInfo -> (Int, Int) -> Either RoundInfo RoundInfo
    go RoundInfo{board} (calledNum, roundNum) =
      let newBoard = fmap (setNum calledNum) <$> board
          roundInfo = RoundInfo{board = newBoard, ..}
       in if isWinner newBoard
            then Left roundInfo -- short-circuit when board wins
            else Right roundInfo

    setNum :: Int -> Int -> Int
    setNum num x = if num == x then -1 else x


isWinner :: Board -> Bool
isWinner board = any (all (<0)) board || any (all (<0)) (transpose board)
