module Problem.FramedWithSilence where

import Common

-- /u/framedwithsilence used the brute-force method just like me
-- but I am impressed by the terseness of his solution taken from
-- here <https://new.reddit.com/r/haskell/comments/rbj981/advent_of_code_2021_day_08/hnpl1ob/>
--
-- Some terseness is lost in translation.
--
-- So, this is similarly brute-forcing it, but it is way more elegant,
-- terse **and** faster than mine.

import Data.List ( elemIndex, permutations, sort )
import Data.Maybe ( fromJust, isJust )

problem1 :: Input -> Output
problem1 ds = length . (>>= filter (flip elem [1, 4, 7, 8])) $ (process <$> ds)

problem2 :: Input -> Output
problem2 ds = sum . map (read . concatMap show) $ (process <$> ds)



process :: Display -> [Digit]
process (Display i o) = map (fromJust . digit . decode i) o

type SegmentDigitGarbled = SegmentDigit
type SegmentDigitClear = SegmentDigit

decode :: Observation -> SegmentDigitGarbled -> SegmentDigitClear
decode x = head . filter (all (isJust . digit) . flip map x)
           $ map wire (permutations "abcdefg")

type Garbling = String -- of exact 7-char length, a permutation of "abcdefg"
wire :: Garbling -> SegmentDigitGarbled -> SegmentDigitClear
wire x = map $ ("abcdefg" !!) . fromJust . flip elemIndex x

digit :: SegmentDigitClear -> Maybe Int
digit = flip elemIndex
        ["abcefg", "cf", "acdeg", "acdfg", "bcdf",
         "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"] . sort