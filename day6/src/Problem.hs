{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
module Problem (problem1, problem2) where

import Common
import Data.List (group, groupBy, sort, sortOn)

problem1 = problem 80 -- we want to get the result after 80 days
problem2 = problem 256

type Days = Int
type Fish = Integer -- the number of fish. We go straight for `Integer`,
                    -- this is clearly an overflow problem now.
type CompactFish = (Days, Fish)
type CompactSwarm = [CompactFish]

problem :: Days -> Input -> Output
problem i fs = countFish $ go i (toHistogram fs)
  where go :: Int -> CompactSwarm -> CompactSwarm
        go 0 fs = fs -- fs stands for "fish (pl.)"
        go i fs | i < 0 = error $ "We can only estimate the number of fish for " <>
                                  "future days, not back into the past!"
                | otherwise = let newFish =  (\(x,y) -> (8,y)) <$> filter ((==0) . fst) fs
                                  agedFish = ageFish <$> fs
                                  newSwarm =  (agedFish <++> newFish)
                              in go (i-1) newSwarm

ageFish :: CompactFish -> CompactFish
ageFish (0,i) = (6,i)
ageFish (x,i) | x < 0 = error "This should not have happened. You screwed up."
              | otherwise = (x-1, i)

countFish :: CompactSwarm -> Integer
countFish fs = sum (snd <$> fs)

-- This was taken from day **5**, /u/brandonchinn178
-- <https://old.reddit.com/r/haskell/comments/r982ip/advent_of_code_2021_day_05/hnaj21z/>
toHistogram :: (Integral b, Ord a) => [a] -> [(a, b)]
toHistogram = map collect . group . sort
  where
    collect xs@(x:_) = (x, fromIntegral (length xs))

-- We certainly could create a newtype and define a semigroup on CompactFish,
-- but that's left for a refactor.
(<++>) :: [CompactFish] -> [CompactFish] -> CompactSwarm
s1 <++> s2 = compress (s1 ++ s2)
  where
    compress :: CompactSwarm -> CompactSwarm
    compress = map sumup . groupBy (\ x y -> fst x == fst y) . (sortOn fst)
    sumup :: [CompactFish] -> CompactFish
    sumup fs@(f:_) = (fst f, sum (snd <$> fs))