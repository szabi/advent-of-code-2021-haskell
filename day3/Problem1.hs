{-# LANGUAGE ScopedTypeVariables #-}
module Problem1 where

import Data.List (transpose)
import Common ( parseBit, evaluate, invert )

problem1 :: [String] -> (Int, Int) -- gamma, epsilon
problem1 input =
        let columns ::[[Int]] = fmap parseBit <$> transpose input
            -- ok, this time we do some validation...
            len1 = length $ head columns
            total = if all (\x -> length x == len1) columns then len1 else undefined
            -- this above validation could be left out:
            -- >>> total = length $ head columns
            gammaBits = fmap (fromEnum . (> 500)) sum <$> columns
            gamma  =  evaluate gammaBits
            epsilon = evaluate $ invert <$> gammaBits
        in (gamma, epsilon)