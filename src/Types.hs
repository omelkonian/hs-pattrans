module Types where

-- | Time in crochet beats.
type Time = Double
-- | MIDI values are represented with integers.
type MIDI = Integer

-- | A piece of music consists of multiple music events.
type MusicPiece = [MirexEntry]
-- | A single entry in a MIREX piece of music.
data MirexEntry = MirexEntry
  { t        :: Time    -- ^ onset time
  , mid      :: MIDI    -- ^ note's MIDI number
  , morph    :: Integer -- ^ note's morphetic pitch number
  , duration :: Time    -- ^ duration in crotchet beats
  , staff    :: Integer -- ^ voice number (starting from 0)
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

-- | A pattern is a sequence of notes.
type Pattern = [Note]
-- | A simplistic music note (only time and pitch).
data Note = Note { ontime :: Time -- ^ onset time
                 , midi   :: MIDI -- ^ MIDI number
                 } deriving (Eq, Show)
