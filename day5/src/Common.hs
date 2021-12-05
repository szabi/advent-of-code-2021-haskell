module Common (
    Coord(..),
    Input,
    Output
) where

type Input = [(Coord, Coord)]
type Output = Int

-- We do not use record syntax so that
-- our parsing code stays simpler
data Coord = Coord Int Int -- x, y
    deriving (Read, Show, Ord, Eq)
