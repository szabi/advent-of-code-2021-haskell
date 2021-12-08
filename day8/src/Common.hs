module Common (
    Input,
    Output,
    Observation,
    Reading,
    Display(..),
    Digit,
    Segment,
    SegmentDigit
) where

type Segment = Char
type SegmentDigit = String
type Observation = [SegmentDigit]
type Reading = [SegmentDigit]
data Display = Display { observation :: Observation, reading :: Reading } deriving (Read, Show)

type Input = [Display]
type Output = Int

type Digit = Int


