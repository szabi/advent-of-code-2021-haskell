{-# LANGUAGE ScopedTypeVariables #-}
module Parse (
    parseLine
) where

import Common
import Data.List.Extra
import Control.Arrow ((>>>))
import Data.Char (toUpper)

-- I'll really need to learn Parsec one day.
-- However, I'm already using way too much time for these
-- advent of code puzzles and spand way too litte with my family
parseLine :: String ->  Display
parseLine s = let line = replace "| " "" (toUpper <$> s)
                  ws = words line
                  wxs :: [String] = sort <$> ws
                  wss = (\s -> "[" <> intersperse ',' s <> "]") <$> wxs
                  ds :: [[Segment]] = (read <$> wss)
              -- the input *always* consists of 10 values before the '|'
              -- and 4 after... so we can just ignore the '|'.
              in  Display (take 10 ds) (drop 10 ds)
