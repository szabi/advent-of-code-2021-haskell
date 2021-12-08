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
parseLine s = let line = replace "| " "" s
                  ws = words line
                  -- we don't need sorting: we have a nice ViewPattern
                  -- wxs :: [String] = sort <$> ws
              -- the input *always* consists of 10 values before the '|'
              -- and 4 after... so we can just ignore the '|'.
              in  Display (take 10 ws) (drop 10 ws)
