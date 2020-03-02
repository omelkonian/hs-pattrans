{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module Discovery where

import Control.Monad (forM_)

import Data.List (isInfixOf)

import Types
import Parser
import Transformations
import EuterpeaUtils
import MIDI (writeToMidi)

type WindowSize = Int
data Query a = ToPattern a => Check Pattern :@ a

upTo :: Time -> Time -> (Time, Time)
upTo = (,)

-- | Query equivalent patterns using a sliding window.
query :: Query a -> MusicPiece -> [Pattern]
query (checker :@ base') piece =
  filter (\p -> (base <=> p) checker)
    $ slide (length base) piece
  where
    base :: Pattern
    base = toPattern piece base'

    slide :: WindowSize -> [a] -> [[a]]
    slide n xs = [ take n (drop m xs) | m <- [0..(length xs - n `max` 0)] ]

queryMatchCount :: Query a -> MusicPiece -> Int
queryMatchCount q mp = length (query q mp)

-- | Example queries.
query1 :: Query (Time, Time)
query1 = (transpositionOf ~~ 0.5) :@ (21 `upTo` 28)

-- for eurovision
query3 :: Query (Music Pitch)
query3 = (transpositionOfPitchOnly ~~ 1.0) :@ (line $ map ($en) [d 5, e 5, d 5, b 4, c 5])

query32 :: Query (Music Pitch)
query32 = (exactOf ~~ 1.0) :@ line [fs 5 en, fs 5 en]

-- for gibbons 
query4 :: Query (Music Pitch)
query4 = (exactOf ~~ 1.0) :@ line [c 5 qn, c 5 qn, d 5 qn, e 5 dhn, f 5 qn]

query42 :: Query (Music Pitch)
query42 = (exactOfRhythmOnly ~~ 1.0) :@ line [c 5 qn, c 5 qn, d 5 qn, e 5 dhn, f 5 qn]

-- for bach
query2 :: Query (Music Pitch)
query2 = (tonalTranspOfCan ~~ 1.0) :@ (line $ map ($qn) [e 5, c 5, f 5, gs 4])

-- for synth data
query5 :: Query (Music Pitch)
query5 = (exactOf ~~ 1.0) :@ line [c 4 qn, d 4 qn, e 4 qn, f 4 qn, g 4 qn]

timeUnitConv :: Note -> Note
timeUnitConv (Note time p) = Note (time * 4) p

infix 0 ??

-- | Query patterns from the given song with given base pattern.
-- e.g. (<dataset>, <song>) ?? query1/query2
(??) :: ToPattern a => (String, String) -> Query a -> IO ()
(s, song) ?? (checker :@ base') = do
  -- which dataset?
  let dataset = head $ filter ((s `isInfixOf`) . datasetName) datasets

  -- parse the music piece
  piece <- (parsePiece dataset) song

  putStrLn $ "Song name is: " ++ show song
  putStrLn $ "Piece length: " ++ show (length piece)

  -- get the base pattern
  let base = map timeUnitConv (toPattern piece base')
  putStrLn $ "Base length: " ++ show (length base)

  -- extract patterns (do not extract the base pattern again)
  let pats = filter (/= base) $ query (checker :@ base) piece
  let firstNotes = map (!! 0) pats
  let ontimes = map ontime firstNotes
  putStrLn $ "Beginnings: " ++ show ontimes
  putStrLn $ "Found patterns: " ++ show (length pats)

  -- export MIDI files
  cd ("data/extracted/" ++ song ++ "/") $ do
    emptyDirectory "."
    writeToMidi "base.mid" base
    forM_ (zip [1..] pats) $
      \(i, p) -> writeToMidi ("occ" ++ show i ++ ".mid") p

queryOneSynth :: ToPattern a => Query a -> IO ()
queryOneSynth q = ("synth", "") ?? q

synthQuery :: ToPattern a => Query a -> IO [()]
synthQuery q = do
  (pieces, names) <- parseMidMusics
  putStrLn $ "File names are: " ++ show names
  putStrLn $ "Checking in this many files: " ++ show (length pieces)

  mapM (\s -> ("queries", s) ?? q) names

-- | Types from which we can extract a pattern from a given song.
class ToPattern a where
  toPattern :: MusicPiece -> a -> Pattern

instance ToPattern Pattern where
  toPattern = const id

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
  toPattern _ = musicToPattern
