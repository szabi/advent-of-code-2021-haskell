module Problem.DemiAoC (problem1, problem2) where


import Common
import Data.List (sort)
problem1 = part1
problem2 = part2

parseInput :: String -> [[Int]]
parseInput = map (map (\x -> read [x])) . lines

rowLength = length . head
colLength = length

point m (x, y) = if 0 <= y && y < colLength m && 0 <= x && x < rowLength m
                 then m !! y !! x
                 else 9

neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isLocalMin :: (Int, Int) -> [[Int]] -> Bool
isLocalMin p@(x, y) m = all ((> point m p) . point m) (neighbours p)

localMinima :: (Int, Int) -> [[Int]] -> [(Int, Int)]
localMinima p@(x, y) m
  | x < rowLength m = if   isLocalMin p m
                      then p : localMinima (x + 1, y) m
                      else     localMinima (x + 1, y) m
  | y < colLength m = localMinima (0, y + 1) m
  | otherwise = []

riskLevel m p = 1 + point m p

part1 m = sum $ map (riskLevel m) $ localMinima (0, 0) m

collectBasin m s p@(x, y) = if   point m p < 9 && p `notElem` s
                            then foldl (collectBasin m) (p:s) (neighbours p)
                            else s

collectBasins m = map (collectBasin m [])

part2 m = let f = product . take 3 . reverse . sort . map (sum . map length)
          in  f $ collectBasins m $ localMinima (0, 0) m
