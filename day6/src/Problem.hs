{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiWayIf #-}

module Problem (problem1, problem2) where

import Common
import Data.MultiSet (MultiSet, size, concatMap, fromList)
import Prelude hiding (concatMap)

problem1 :: Input -> Output
problem1 fs = countAllFish (swarmAtDay fs !! 80) -- we want to get the result after 80 days
problem2 :: Input -> Output
problem2 fs = countAllFish (swarmAtDay fs !! 256)

type Fish = Int
type CompactSwarm = MultiSet Fish -- the keys are the age in days of the fish.

-- This was translated from /u/StephenSwat's solution
-- <https://www.reddit.com/r/haskell/comments/r9z4qb/advent_of_code_2021_day_06/hng5ohl/>
--
-- using `MultiSet` is even more elegant than using a `Map`!

swarmAtDay :: [Fish] -> [CompactSwarm]
swarmAtDay fs = iterate advanceDay (fromList fs)

advanceDay :: CompactSwarm -> CompactSwarm
advanceDay = concatMap (\i -> if i == 0 then [6, 8]
                                        else [i - 1])

-- this is just a rename to make lines 12 and 13 even more
-- directly readable
countAllFish :: CompactSwarm -> Fish
countAllFish = size

-- /u/Tarmen's solution <https://www.reddit.com/r/haskell/comments/r9z4qb/advent_of_code_2021_day_06/hng4n4p/>
-- is also nice.