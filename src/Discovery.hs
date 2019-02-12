{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module Discovery where

import Control.Monad (forM_)

import Types
import Parser
import Transformations
import EuterpeaUtils
import MIDI (writeToMidi)

type WindowSize = Int
type Query a = (Check a, a)
data UserQuery a = ToPattern a => Check Pattern :@ a

upTo :: Time -> Time -> (Time, Time)
upTo = (,)

-- | Query equivalent patterns using a sliding window.
query :: Query Pattern -> MusicPiece -> [Pattern]
query (checker, base) =
  filter (\p -> (base <=> p) checker) . slide (length base)
  where
    slide :: WindowSize -> [a] -> [[a]]
    slide n xs = [ take n (drop m xs) | m <- [0..(length xs - n `max` 0)] ]

-- | Example queries.
query1 :: UserQuery (Time, Time)
query1 = (transpositionOf ~~ 0.5) :@ (21 `upTo` 28)

query2 :: UserQuery (Music Pitch)
query2 = (transpositionOf ~~ 0.5) :@ (line $ map ($qn) [c 4, e 4, g 4, c 5])

-- | Query patterns from the given song with given base pattern.
-- e.g. "bach" ?? query1/query2
(??) :: ToPattern a => Song -> UserQuery a -> IO ()
infix 0 ??
song ?? q :@ base' = do
  -- parse the music piecec
  piece <- parseMusic song
  putStrLn $ "Piece length: " ++ show (length piece)

  -- get the base pattern
  let base = toPattern piece base'
  putStrLn $ "Base length: " ++ show (length base)

  -- extract patterns (do not extract the base pattern again)
  let pats = filter (/= base) $ query (q, base) piece
  putStrLn $ "Found patterns: " ++ show (length pats)

  -- export MIDI files
  cd ("data/extracted/" ++ song ++ "/") $ do
    emptyDirectory "."
    writeToMidi "base.mid" base
    forM_ (zip [1..] pats) $
      \(i, p) -> writeToMidi ("occ" ++ show i ++ ".mid") p

-- | Types from which we can extract a pattern from a given song.
class ToPattern a where
  toPattern :: MusicPiece -> a -> Pattern

-- | Given a song name, one can extract a musical pattern
-- by parsing the song file and selecting some time period.
instance ToPattern (Time, Time) where
  toPattern song (startT, endT) =
    ( takeWhile ((<= endT)  . ontime)
    . dropWhile ((< startT) . ontime)
    ) song

-- | Given a datatype that can be converted to Euterpea's core Music datatype,
-- one can subsequently convert that to get a musical pattern.
instance ToMusic1 a => ToPattern (Music a) where
  toPattern _ x = musicToPattern (toMusic1 x)
