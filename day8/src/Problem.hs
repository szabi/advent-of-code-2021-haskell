{-# LANGUAGE ViewPatterns #-}
module Problem (problem1, problem2) where

import Common
import Data.List (sort, permutations)
import Data.Maybe (fromJust)

problem1 :: Input -> Output
problem1 ds = let x =  concat $ countEasy <$> ds
              in length x
  where countEasy (Display _ reading) = filter crit reading
        crit ss = length ss `elem` [2,3,4,7]

problem2 :: Input -> Output
problem2 ds = let ns = digitsToDecimal . problem2display <$> ds
              in sum ns

digitsToDecimal = foldl (\x y -> x * 10 + y) 0

problem2display :: Display -> [Int]
problem2display (Display observation reading) =
    let tryencodings = [ (enc, decode enc <$> observation) | enc <- [0..(length alldecodings - 1)] ]
        correct = filter (allValid . snd)  tryencodings
        corrEnc =  fst $ head correct
    in map fromJust (decode corrEnc <$> reading)
    where allValid = notElem Nothing

type Encoding = Int -- actually lenght allencodings

decode :: Encoding -> SegmentDigit -> Maybe Int
decode n sd = fromEnum <$> segmentsToDigits (dec n <$> sd)

dec :: Encoding -> Segment -> Segment
dec n s = (alldecodings !! n) !! fromEnum s

origsequence =  [A, B, C, D, E, F, G]
alldecodings = permutations origsequence



segmentsToDigits :: [Segment] -> Maybe Digit
segmentsToDigits (sort -> [A, B, C, E, F, G])    = Just Zero
segmentsToDigits (sort -> [C, F])                = Just One     --
segmentsToDigits (sort -> [A, C, D, E, G])       = Just Two
segmentsToDigits (sort -> [A, C, D, F, G])       = Just Three
segmentsToDigits (sort -> [B, C, D, F])          = Just Four    --
segmentsToDigits (sort -> [A, B, D, F, G])       = Just Five
segmentsToDigits (sort -> [A, B, D, E, F, G])    = Just Six
segmentsToDigits (sort -> [A, C, F])             = Just Seven   --
segmentsToDigits (sort -> [A, B, C, D, E, F, G]) = Just Eight   --
segmentsToDigits (sort -> [A, B, C, D, F, G])    = Just Nine
segmentsToDigits _                               = Nothing

-- A B C D E F G
--     x     x   -- one
-- x   x     x   -- seven
--   x x x   x   -- four
-- x x x x x x x -- eight