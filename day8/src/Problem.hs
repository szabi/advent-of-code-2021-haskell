{-# LANGUAGE ViewPatterns #-}
module Problem (problem1, problem2) where

import Common
import Data.List (sort, permutations)
import Data.Maybe (fromJust)
import Data.Char (ord)

problem1 :: Input -> Output
problem1 ds = let x =  concat $ countEasy <$> ds
              in length x
  where
    countEasy :: Display -> [String]
    countEasy (Display _ reading) = filter crit reading
    crit :: String -> Bool
    crit ss = length ss `elem` [2,3,4,7]

problem2 :: Input -> Output
problem2 ds = sum (digitsToDecimal . problem2display <$> ds)
  where
    digitsToDecimal :: [Int] -> Int
    digitsToDecimal = foldl (\x y -> x * 10 + y) 0

problem2display :: Display -> [Int]
problem2display (Display observation reading) =
    let tryencodings = [ (enc, decode enc <$> observation)
                       |  enc <- [0..(length alldecodings - 1)]
                       ]
        correct = filter (allValid . snd)  tryencodings
        -- I dislike both this `head` and the `fromJust` below, but given non-corrupt
        -- input this is a correct program which can never throw an error.
        corrEnc =  fst $ head correct
    -- we *know* that we can use `fromJust` here: we just filtered for that one encoding
    -- which works.
    in map fromJust (decode corrEnc <$> reading)
    where allValid = notElem Nothing

type Encoding = Int -- actually up to (lenght allencodings - 1)

decode :: Encoding -> SegmentDigit -> Maybe Int
decode n sd = segmentsToDigits (dec n <$> sd)

dec :: Encoding -> Segment -> Segment
dec n s = (alldecodings !! n) !! (ord s - ord 'a')

origsequence =  "abcdefg"
alldecodings = permutations origsequence


segmentsToDigits :: SegmentDigit -> Maybe Digit
segmentsToDigits (sort -> "abcefg")  = Just 0
segmentsToDigits (sort -> "cf")      = Just 1   --
segmentsToDigits (sort -> "acdeg")   = Just 2
segmentsToDigits (sort -> "acdfg")   = Just 3
segmentsToDigits (sort -> "bcdf")    = Just 4   --
segmentsToDigits (sort -> "abdfg")   = Just 5
segmentsToDigits (sort -> "abdefg")  = Just 6
segmentsToDigits (sort -> "acf")     = Just 7   --
segmentsToDigits (sort -> "abcdefg") = Just 8   --
segmentsToDigits (sort -> "abcdfg")  = Just 9
segmentsToDigits _                   = Nothing
