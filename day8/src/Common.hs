module Common (
    Input,
    Output,
    Observation,
    Reading,
    Display(..),
    Digit(..),
    Segment(..),
    SegmentDigit
) where

type SegmentDigit = [Segment]
type Observation = [SegmentDigit]
type Reading = [SegmentDigit]
data Display = Display { observation :: Observation, reading :: Reading } deriving (Read, Show)
type Input = [Display]
type Output = Int

data Digit = Zero
           | One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
    deriving (Enum, Show)

data Segment = A | B | C | D | E | F | G deriving (Enum, Show, Eq, Ord, Read)

