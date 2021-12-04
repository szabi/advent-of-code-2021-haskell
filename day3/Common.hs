module Common (
    parseBit,
    evaluate,
    invert
) where

import Data.List (singleton)

--- While we could write `invert x = (1 - x)`, we make it slightly more
--- robust: we'll notice if we made a mistake. In a large application
--- `undefined` is usually *less* robust, not so in this scripty solution.
invert :: Int -> Int
invert 0 = 1
invert 1 = 0
invert _ = undefined


-- as always with these AoC problems, we approach this scripty.
-- we assume input is not malformed, so we can use "read"
-- and assume only '1' or '0' exists in the input
--
-- Seriously? `singleton` is only available from base-4.15? => I needed to switch to GHC 9.0.1 
parseBit :: (Enum a) => Char -> a
parseBit = toEnum . read . singleton

-- twelve bits easily fit into an `Int`
-- what about the highest possible value for our solution?
--
-- >>> (evaluate $ take 12 $ repeat 1) ^ 2
-- 16769025
--
-- >>> maxBound :: Int
-- 9223372036854775807
--
-- yeah, that should do.
evaluate :: (Enum a) => [a] -> Int
evaluate = foldl step 0
  where
      step :: (Enum a) => Int -> a -> Int
      step acc bit = acc * 2 + fromEnum bit
