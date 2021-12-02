module Main where
import Data.Char (toUpper)

data Command n = Forward n | Up n | Down n
    deriving (Read)

type Position = (Int, Int) -- horizontal and DEPTH
data Aim = Aim {position :: Position, aim :: Int }
    deriving (Show)

-- isn't it nice that our input file lends itself to direct
-- parsing by 'read'?
parseLine :: String -> Command Int
parseLine = read . upperFirst
   where upperFirst (c:cs) = toUpper c : cs
         upperFirst _ = ""

-- I was playing with the idea to use a class or a type family
-- over Position and Aim, but no way I am going to overcomplicate this.
executeCommand1 :: Position -> Command Int -> Position
executeCommand1 (x, d) (Forward n) = (x + n, d)
executeCommand1 (x, d) (Up      n) = (x, d - n)
executeCommand1 (x, d) (Down    n) = (x, d + n)

executeCommand2 :: Aim -> Command Int -> Aim
executeCommand2 (Aim (x, d) a) (Forward n) = Aim (x + n, d + n * a) a
executeCommand2 (Aim (x, d) a) (Up      n) = Aim (x, d) (a - n)
executeCommand2 (Aim (x, d) a) (Down    n) = Aim (x, d) (a + n)

-- just specializing ... for readability and understandability
-- @a@ can be either 'Position' or 'Aim'
dayTwoProblem :: (a -> Command Int -> a) -> a -> [Command Int] -> a
dayTwoProblem = foldl

-- we are defining this function really just for the inline test.
-- did I say I LOVE 'hls'?
-- >>> dayTwoProblem2 [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]
-- Aim {position = (15,60), aim = 10}
dayTwoProblem2 ::  [Command Int] -> Aim
dayTwoProblem2 = dayTwoProblem executeCommand2 (Aim (0,0) 0)

-- but once we did this, we can also factor problem1 out, ex-post
dayTwoProblem1 :: [Command Int] -> Position
dayTwoProblem1 = dayTwoProblem executeCommand1 (0,0)

main :: IO ()
main = do
        content <- readFile "input.txt"
        let commands = parseLine <$> lines content
        --
        let problem1 = dayTwoProblem1 commands
        putStrLn $ "Day 2, Puzzle 1: " <> show (uncurry (*) problem1)
        --
        let problem2 = dayTwoProblem2 commands
        putStrLn $ "Day 2, Puzzle 2: " <> show (uncurry (*) (position problem2))
