{-# LANGUAGE ScopedTypeVariables #-}
module Problem2.Topaz (problem2) where

import Common (parseBit, evaluate)
import Data.List (transpose, maximumBy, minimumBy, group, sort)
import Data.Ord (comparing)
-- inspired by
-- https://topaz.github.io/paste/#XQAAAQCLBwAAAAAAAAA2m8ixrhLu7YJFrd2FLde+PAG1Aui2yN36LC93WIQ2APL8FwxSxSJjfSaqoOGd73rIqpWWzsCMphcPUhOzUs5gHOzjdUvmitH82i5/qUBbjp+h+OGYwyiIcNDnhlkCcw0QzU47Hi/lqdhWEtjZMJj4TX237QVVQkSf9qSRZTZnDwmnoRxjfncvzIKsLWFb4acDFahRw29RFj7lXgb8mJ8oqMoYwSFuMkkOZn0v4R9hEZPrBWzkYf1dbowIqtYjfDmP6LFzHtx/nMw5+TlID0jqTEHO6FkQ9GOD5Siaxm6MM24g0r84/uc5ZOSjkzP5456dhJiOApjbC0seX9vFZs2e8aXYezoWukQSNtlF2R+9vn4YwncUKbIo2ypgIsBvmBGbi+xSg2vSx3XldyXeVLQo6ljuBs4w+r/8r2tFkYANERWgbZYWSBSKfrt8lgwjopFi4xClDyJp75v8xGz6XlVyCWstHeXTbjn4HPvZXSo1lqLvGjgYXtQ6B1t4+Ahm9yKQuG+bjmHmY/wm27mBUFxWA0KKaa8s4o5ECAI8JCmcB5mEO1p2x3sv7NuQJDsQ9H2P4kT36ddpqN30XK+cf3dT1G/pZUDtVqXrX/3zoPoqk/aZRtUh2oimYDbPEk1F+9flMnBYSiHUzRatslkI0sqf7onMSlU3LP50Z5k5KnX4DMY348zbBp4M6r0xxK578+zcf0Eb88LoQY7rbJcJw4a9kxitDlmOiZIW6jQj9J64XkP1Ru6MCWRx5pYvAftQQUE=


type Byte = [Int] -- shall only cotain 1s and 0s


filterBy :: ([Int] -> Int) -> [[Int]] -> [Int]
filterBy criterion all@(x : xs) = let selected = criterion x in
    selected : filterBy criterion (tail $ transpose $ filter (\num -> head num == selected) (transpose all))
filterBy _ [] = []


mostCommon :: [Int] -> Int
mostCommon x = head . maximumBy (comparing length) . group $ sort x
leastCommon :: [Int] -> Int
leastCommon x = head . minimumBy (comparing length) . group $ sort x


problem2 :: [String] -> (Int, Int) -- oxygen, co2
problem2 input =
    let bytes :: [Byte] = fmap parseBit <$> input
        wires = transpose bytes
    in ( evaluate $ filterBy mostCommon wires,
         evaluate $ filterBy leastCommon wires)

