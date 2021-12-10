{-# LANGUAGE RecordWildCards #-}

module Problem (problem1, problem2) where

import Common
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust, mapMaybe)

problem1 = run problem1def
problem2 = run problem2def

data ProblemDef a = ProblemDef {
  finalScore :: [Int] -> Int,
  lineScore :: a -> Int,
  lineProcess :: String -> Maybe a
}

problem2def :: ProblemDef String
problem2def = ProblemDef {
  finalScore = \ns -> sort ns !! (length ns `div` 2),
  lineScore = foldl (\acc c -> acc * 5 + completionScoreOf c) 0,
  lineProcess = completeStack <.> lineStack
}

problem1def :: ProblemDef Char
problem1def = ProblemDef {
  finalScore = sum,
  lineScore = closing `mapTo` errorScores,
  lineProcess = lineError
}

run :: ProblemDef a -> [String] -> Int
run ProblemDef{..} ss = finalScore (lineScore <$> mapMaybe lineProcess ss)

type Stack = String
type Complement = String

opening, closing :: [Char]
errorScores, completionScores :: [Int]
opening = "([{<"
closing = ")]}>"
errorScores = [3,57,1197,25137]
completionScores = [1,2,3,4]
-- originally I had a function essentially saying `elemIndex + 1`,
-- but we don't need performance here, and so the code becomes more
-- uniform, easier to read and understand, less cluttered

lineError :: String -> Maybe Char
-- returns Nothing if there is no stacking error,
-- returns the violating character if there is a stacking error.
lineError = leftToMaybe . buildStack

lineStack :: String -> Maybe String
-- returns Nothing if there is a stack error,
-- returns a `Just s` if there is a valid completion
lineStack = rightToMaybe . buildStack

-- *
-- * Stack processing
-- *

buildStack :: String -> Either Char Stack
buildStack = foldl addTostack (Right "")

addTostack :: Either Char Stack -> Char -> Either Char Stack
addTostack l@(Left c) _ = l
addTostack (Right s) c'
    | c' `notElem` opening && c' `notElem` closing = error $ "stack: invalid input, must be one of " <> show opening <> " or " <> show closing
    | c' `elem` opening = Right (c' : s)
    | c' `elem` closing = case s of
        (c : cs) -> if matching c c' then Right cs
                                     else Left c'
        _        -> Left c'

completeStack :: Stack -> Complement
completeStack = foldl complete ""
  where
    complete :: Complement -> Char -> Complement
    complete s c = s ++ [closingOf c]

-- *
-- * Helper and readability
-- *

matching :: Char -> Char -> Bool
matching c c' = elemIndex c opening == elemIndex c' closing

closingOf :: Char -> Char
closingOf = opening `mapTo` closing

completionScoreOf :: Char -> Int
completionScoreOf = closing `mapTo` completionScores

mapTo :: (Eq a, Show a) => [a] -> [b] -> (a -> b)
(source `mapTo` target) a
    | a `elem` source = target !! fromJust (elemIndex a source)
    | otherwise = error $ "invalid char: " <> show a <> "', must be one of " <> show source

-- no way I'm pulling in "semigroupoids" for package "either"

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g