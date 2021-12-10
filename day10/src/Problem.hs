{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Problem (problem1, problem2) where

import Common
import GHC.IO.Handle.Text (commitBuffer')
import Data.List (elemIndex)
import Data.Maybe (catMaybes, fromJust, mapMaybe)

problem2 = undefined

problem1 :: [String] -> Int
problem1 ss = sum (scoreError <$> mapMaybe lineError ss)

type Stack = String

opening = "([{<"
closing = ")]}>"

scoresError :: [Int]
scoresError = [3,57,1197,25137]

scoreError :: Char -> Int
scoreError c | c `elem` closing = scoresError !! fromJust (elemIndex c closing)
        | otherwise = error $ "score: invalid char: " <> show c

lineError :: String -> Maybe Char
-- returns Nothing if there is no stacking error,
-- returns the violating character if there is a stacking error.
lineError = leftToMaybe . foldl stack (Right "")

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

stack :: Either Char Stack -> Char -> Either Char Stack
stack l@(Left c) _ = l
stack (Right s) c'
    | c' `notElem` opening && c' `notElem` closing = error $ "stack: invalid input, must be one of " <> show opening <> " or " <> show closing
    | c' `elem` opening = Right (c' : s)
    | c' `elem` closing = case s of
        (c : cs) -> if elemIndex c' closing == elemIndex c opening
                    then Right cs
                    else Left c'
        _        -> Left c'