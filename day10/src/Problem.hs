{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Problem (problem1, problem2) where

import Common
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust, mapMaybe)

problem2 :: [String] -> Int
problem2 ss = scoreComplete'' $ scoreComplete' . completeStack <$> mapMaybe lineComplete ss

problem1 :: [String] -> Int
problem1 ss = sum (scoreError <$> mapMaybe lineError ss)

type Stack = String
type Complement = String

opening = "([{<"
closing = ")]}>"

errorScores :: [Int]
errorScores = [3,57,1197,25137]

scoreError :: Char -> Int
scoreError = closing `mapTo` errorScores

scoreComplete :: Char -> Int
scoreComplete c = case elemIndex c closing of
    Nothing -> error $ "scoresComplete: invalid char '" <> show c <> "', must be one of " <> show closing
    Just n  -> n + 1

scoreComplete' :: String -> Int
scoreComplete' = foldl (\acc c -> acc * 5 + scoreComplete c) 0

scoreComplete'' :: [Int] -> Int
scoreComplete'' ns = sort ns !! (length ns `div` 2)

lineError :: String -> Maybe Char
-- returns Nothing if there is no stacking error,
-- returns the violating character if there is a stacking error.
lineError = leftToMaybe . buildStack

lineComplete :: String -> Maybe String
-- returns Nothing if there is a stack error,
-- returns a `Just s` if there is a valid completion
lineComplete = rightToMaybe . buildStack

buildStack :: String -> Either Char Stack
buildStack = foldl stack (Right "")

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

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

completeStack :: Stack -> Complement
completeStack = foldl complete ""
  where
    complete :: Complement -> Char -> Complement
    complete s c = s ++ [closingOf c]

closingOf :: Char -> Char
closingOf = opening `mapTo` closing

mapTo :: (Eq a, Show a) => [a] -> [b] -> (a -> b)
(source `mapTo` target) a
    | a `elem` source = target !! fromJust (elemIndex a source)
    | otherwise = error $ "invalid char: " <> show a <> "', must be one of " <> show source
