module Main where

type Position = (Int, Int)  -- horizontal, depth

main :: IO ()
main = do
        content <- readFile "input.txt"
        let d = (read <$> lines content) :: [Int]
        --
        let increases1 = method1 d
        print $ "Day 1, Puzzle 1, Version 1: " <> show increases1
        let increases2 = method2 d
        print $ "Day 1, Puzzle 1, Version 2: " <> show increases2
        -- puzzle 2 of this day:
        let slidingwindow = sliding d
        print $ "Day 1, Puzzle 2, Version 2: " <> show slidingwindow

-- this would actually be @(Ord b, Num a) => [b] -> a@,
-- but we are restricting it for clarity
method2 :: [Int] -> Int
method2 d = fst $ foldl counter (0, head d) (tail d)
  where
      counter (i, old) next
        | old < next = (i + 1, next)
        | otherwise  = (i    , next)

method1 :: [Int] -> Int
method1 d = foldl counter 0 $ zip d (tail d)
  where
    counter :: Int -> Position -> Int
    counter i (a,b)
        | a < b     = i + 1
        | otherwise = i

sliding :: [Int] -> Int
sliding d = method2 sl
  where sl = map (\(a,b,c) -> a + b + c) $ zip3 d (drop 1 d) (drop 2 d)