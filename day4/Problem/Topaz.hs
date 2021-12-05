module Problem.Topaz (problem1, problem2) where

import Common
import Data.List (transpose, minimumBy, maximumBy)
import Data.Ord (comparing)

-- translated from
-- <https://topaz.github.io/paste/#XQAAAQAjDAAAAAAAAAA2m8ixrhLu7YJFrd2FLde+PAG1Aui2yN36LC93botUXBAmzbUPu6FdtzNp4jgmhfLmgk4kJsPvp/H24mnOJvgWbDd8eqtgzy2PEk7Dh5/rmdWK7W7snwL0MXKU719qa2RFuO5/zgJqVOtuUgzah13MEopYrZlV0vVsf1Lkr7tnwIAc6JNlQPz6ryjdJFaSo7mkJcQYwlqLwyo+F8MFTnDHG5aQOH3PKdWHF0A4XErU6Uu0hkZRT4Qx5Cy00s7uc/wFq2KlSb3/4bcsjpP6Xq++Ji6SI5kKiiKrtxjZ1/1dNr4D5VyfTCLIcsQXbelfhSA6YeesKoZ34WbnhpDVIg5JEYfVtibaFVge54B6qwcsm4Coh4dXIepsRlO06FebOqmvUbKmO9UwkV3aLkHVmU3YMWJ5le97YRc1cxyfj34xuG0RPekPFbi6uWzKYU02haBqqI7jTd6DFwVLMyARu9V7536fP1itddpGrkiYeNop9s0xtlc9X/IonRDnMw084jagaqznS1AYnkRgKh+JUlCXfzg8bfMEGSZJmJ8QSNtMQwkrwMHQ6vELUFNT1zg0RF1U7/zyY2RFKZ9LnwxRTcWGN9ObhlNZbL+AtzikDQP9n5AsNfPhKep+YhhJflr1+U62DjjzDKXX3fZo4kaH9w6StAKSKp70v5QzDqh6T+dkfoidvQJndNbpGdGqOzGjyGjw6RTBBvKkvIrzBiCNyOjZ6VE7Az+LZ9Oh4X2t/9wDSD4lO1D6py+ukRS4TFlTaYJbfIRHlvFAaK7ivQxpgIXWavewFEHvl31vN1JQYl1oNWymU9Mx+ifVd7XH4qdwYY2Woo8r+CfYi7nTL0F1jpAfDYR1qc4xhdrFtnForighYul7McZcRJuu9jp++FS7qox3njmdhMekbPbFbt0jW78tb88UWKbXUchACLxXBnQ1Kb53fsftZR4GmCl4q2yUJKDMz7D3BWY1c4d9HitSKVjJ56V5pIdZxZJFFqufge8TXnfhPItDE34eXQx/SzvfK78pKjBwdTf3m00ND7g4OYPiDWaS69aON0p1/NtBTqwDGun/Cv1aVDIsQFH2NhJwxtZPGnIiryZLI+iczYceGZEVzl0OHbx6sxjdPdcI3Eo8Qn/NQhevXc+Nqz9G+zb6JhXjmQ6kbg2CpMkz9HGJRzigkFE1gA5J++A4EjeHVcpb/BFm2cSpvGy2ul91ZX8praHsMmVv/sHLmg==>
--
-- An interesting approach, different than most. Again, I'm not particularly impressed by its
-- structure or readability, but I'd like to study it. Definitely a novel approach.
--
-- Copmare to JHidding!

type SelectionCriterion = (Output -> Output  -> Ordering) -> [Output] -> Output

problem1 :: [Int] -> [Board] -> Output
problem1 = selectBoard firstWinner
    where firstWinner = minimumBy
problem2 :: [Int] -> [Board] -> Output
problem2 = selectBoard lastWinner
    where lastWinner = maximumBy

selectBoard :: SelectionCriterion -> [Int] -> [Board] -> Output
selectBoard selector order bingos = ( selectedBingo  , order !! (idx - 1) )
 where
  (selectedBingo, idx) =
    selector (comparing snd) $ map (`getFirstBingo` order) bingos

getFirstBingo :: Board -> [Int] -> Output
getFirstBingo bingo =
  head . filter (winning . fst) . (`zip` [0 ..]) . scanl (flip updateWith) bingo
