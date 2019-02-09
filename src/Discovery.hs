module Discovery where

import Types
import Transformations

type WindowSize = Int
type TimePeriod = (Time, Time)
data UserQuery a = Check a :@ TimePeriod
type Query a = (Check a, a)

-- | An example query.
testQuery :: UserQuery Pattern
testQuery = (transpositionOf ~~ 0.5) :@ (21, 28)

-- | Query equivalent patterns using a sliding window.
query :: Query Pattern -> MusicPiece -> [Pattern]
query (checker, base) =
  filter (\p -> (base <=> p) checker) . slide (length base)
  where
    slide :: WindowSize -> [a] -> [[a]]
    slide n xs = [ take n (drop d xs) | d <- [0..(length xs - n `max` 0)] ]

