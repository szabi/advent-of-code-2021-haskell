module Parse (
    parseLine
) where

import Common
import Data.List.Extra (replace)
import Control.Arrow ( (>>>) )

-- I'll really need to learn Parsec one day.
-- However, I'm already using way too much time for these
-- advent of code puzzles and spand way too litte with my family
parseLine :: String ->  (Coord, Coord)
parseLine s =
    let readable = "(Coord " <>
                   (replace "," " " >>> replace " -> " ", Coord ") s
                   <> ")"      
    in read readable