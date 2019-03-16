module Types where

import Data.List (intersperse)
import Control.Parallel.Strategies (parMap, rpar)
import Control.Concurrent.Async (forConcurrently, mapConcurrently)

-- | Time in crochet beats.
type Time = Double
-- | MIDI values are represented with integers.
type MIDI = Integer
-- | Intervals are represented with integers (i.e. number of semitones).
type Interval = Integer

-- | A pattern group is one of the patterns of a piece of music, identified by an expert
-- or algorithm, and defined by a pattern prototype and other pattern occurences.
data PatternGroup = PatternGroup
  { piece_name   :: String
  -- ^ the name of the music piece, that the pattern group belongs to
  , expert_name  :: String
  -- ^ music expert or algorithm that produced the pattern occurences
  , pattern_name :: String
  -- ^ the name of the current pattern group
  , basePattern  :: Pattern
  -- ^ the pattern prototype (always taken from `occ1.csv`)
  , patterns     :: [Pattern]
  -- ^ all other pattern occurences of the prototype
  } deriving (Eq)

instance Show PatternGroup where
  -- | Get a title, unique to the given PatternGroup, in the following format:
  -- <piece>:<expert>:<pattern>.
  show (PatternGroup piece_n expert_n pattern_n _ _) =
    concat $ intersperse ":" [piece_n, expert_n, pattern_n]

-- | A pattern is a sequence of notes.
type Pattern = [Note]

-- | A simplistic music note (only time and pitch).
data Note = Note { ontime :: Time -- ^ onset time
                 , midi   :: MIDI -- ^ MIDI number
                 } deriving (Eq, Show)

-- | Infix variant of the Note constructor.
(.@) :: Time -> MIDI -> Note
(.@) = Note

-- | A piece of music is a huge pattern.
type MusicPiece = Pattern

-- | Songs are identified with a string.
type Song = String

-----------------------
-- Parallel operations

-- | Parallel map.
pmap :: (a -> b) -> [a] -> [b]
pmap = parMap rpar

-- | Parallel forM.
pforM :: Traversable t => t a -> (a -> IO b) -> IO (t b)
pforM = forConcurrently

-- | Parallel mapM.
pmapM :: Traversable t => (a -> IO b) -> t a -> IO (t b)
pmapM = mapConcurrently
