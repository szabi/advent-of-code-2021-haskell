{-# LANGUAGE ScopedTypeVariables #-}
-- This language extension allows us to annotate variables with type
-- information on the left hand side. This is done for readability
-- and understandabiltiy in this case.
--
-- HLS does provide type information on hover, but sometimes it's
-- easier and more immediate to just see in what stage of processing
-- we are.

module Problem1.Overengineered (
    problem1
) where

import Common ( parseBit, evaluate )
-- we could use the `bitvec` package, but we are not strapped for efficiency.

-- We could actually work with `Bool`, or even `bitvec::Bit`, but let's not
-- overcomplicate this
data Bit = Zero | One deriving (Enum, Read)
invert :: Bit -> Bit
invert Zero = One
invert One = Zero

-- did you know that 'byte' means "an architecture-defined grouping of bits", and not
-- necessarily *eight* bits? It's just that todays chip architectures (applications)
-- all use octets as bytes... This submarine is obviously a 12-bit system:
-- https://en.wikipedia.org/wiki/12-bit_computing
type Byte = [Bit]

data BitCount = BitCount { zero :: Int, one :: Int } deriving (Show)
-- Let BitCount be component-wise additive semigroup:
instance Semigroup BitCount where
    BitCount a b <> BitCount x y = BitCount (a + x) (b + y)
-- we could actually define a zero element for it, but we won't
-- need it in this applicaiton:
-- instance Monoid BitCount where
--     mempty = BitCount 0 0

-- essentially an embedding, a smart constructor
bitCount :: Bit -> BitCount
bitCount bit = BitCount (fromEnum (invert bit)) (fromEnum bit)

type ByteCount = [BitCount]

-- This is an embedding, a smart constructor, analogously
-- of course we could inline this, but it will make our
-- code more readable.
byteCount :: Byte -> ByteCount
byteCount = fmap bitCount

-- And this a projection
-- well, actually problem 1 is mis-specified!
-- it says, "gamma rate" comes from the *most common* bit
-- and "epsilon rate" from the *least common*... but there are
-- exactly 1000 data points! So while fortunately there is no
-- bit column with a 500:500 ratio in our actual input, in a
-- "real" set of readings there could be!
consensus :: BitCount -> Bit
consensus (BitCount z o)  | z < o = One
                          | z > o = Zero
                          | otherwise = undefined -- Shame on you, AoC problem authors!

problem1 :: [String] -> (Int, Int) -- gamma, epsilon
problem1 input =
        let inputBytes :: [Byte] = fmap parseBit <$> input
            listOfByteCount :: [ByteCount] = fmap byteCount inputBytes
            -- we summarize our list of bytecounts
            readingsStats = foldr1 (zipWith (<>)) listOfByteCount
            gamma = evaluate $ consensus <$> readingsStats
            -- Even though #zeros = #ones is unspecified, we can simply invert
            -- gamma to get epsilon, because we'd already be in an undefined
            -- state for gamma... (see `consensus`)
            epsilon = evaluate $ invert . consensus <$> readingsStats
        in (gamma, epsilon)