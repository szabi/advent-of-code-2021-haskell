module Problem.Entertainment (problem1, problem2) where

import Common

problem1 = problem id
problem2 = problem increasingCost

-- Yeah, doing a bisection was overengineering, because I assumend problem 2
-- will again be some tractability / computational complexity problem.
--
-- No need for that.
--
-- Just brute force it!
--
-- This brute force formulation using *list comprehension* was inspired by
-- u/EntertainmentMuch818 :
-- <https://old.reddit.com/r/haskell/comments/rar8eg/advent_of_code_2021_day_07/hnk6n13/>
--
-- HOWEVER: this direct implementation is measurably and *perceivably* slower!

type Distance = Int
type Cost = Int
type FuelCostFn = Distance -> Cost

problem :: FuelCostFn -> Input -> Output
problem metric nums = minimum
--[ sum [ metric $ abs (num - d) | d <- nums ]
  [ absDiff metric num nums
  | num <- [minimum nums .. maximum nums]
  ]

-- I like this formulation as well, compare it to my original
-- using function compositon etc.
absDiff :: FuelCostFn -> Distance -> Input -> Cost
absDiff metric m xs = sum [ metric $ abs (m - d) | d <- xs ]

distances :: [Distance]
distances = [1..]

costs :: [Cost]
costs = scanl (+) 0 distances

increasingCost :: FuelCostFn
increasingCost x = costs !! x