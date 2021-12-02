module Main where

type Position = (Int, Int)  -- horizontal, depth

main :: IO ()
main = do
        content <- readFile "input.txt"
        let d = (read <$> lines content) :: [Int]
        --
        let increases1 = problem1 d
        print $ "Day 1, Puzzle 1: " <> show increases1
        -- puzzle 2 of this day:
        let slidingwindow = problem2 d
        print $ "Day 1, Puzzle 2: " <> show slidingwindow

-- For one, I have overlooked that (a + b + c) < (b + c + d) <==> a < d.
-- with that, our solution can be reduced to one and the same problem

-- The second improvement is to use `zipWith`! and then sum up the @Bool@s...

-- this would actually be @(Ord b, Num a) => [b] -> a@,
-- but we are restricting it for clarity
method :: Int -> [Int] -> Int
method window d =  sum $ oneIfTrue <$> zipWith (<) d (drop window d)
  where oneIfTrue = fromEnum

problem2 :: [Int] -> Int
problem2 = method 3

problem1 :: [Int] -> Int
problem1 = method 1