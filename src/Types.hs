module Types where

import Control.Parallel.Strategies (parMap, rpar)
import Control.Concurrent.Async    (forConcurrently, mapConcurrently)

import Data.List (intersperse, maximumBy)
import qualified Data.Map  as M
import qualified Data.Set  as S

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
(.@) :: MIDI -> Time -> Note
(.@) = flip Note

-- | A piece of music is a huge pattern.
type MusicPiece = Pattern

-- | Songs are identified with a string.
type Song = String

-----------------------
-- Utilities

-- | Negate the values of a numeric list.
inverse :: Num a => [a] -> [a]
inverse = fmap negate

-- | The base pitch of a pattern (the pitch of its first note).
-- e.g. basePitch [(25,1), (27,2), (25,2.5)] = Just 25
basePitch :: Pattern -> Maybe MIDI
basePitch (Note _ m:_) = Just m
basePitch []           = Nothing

-- | The (real) pitch structure of a pattern.
-- e.g. pitch [(25,1), (27,2), (25,2.5)] = [25, 27, 25]
pitch :: Pattern -> [MIDI]
pitch = fmap midi

-- | The (relative) pitch structure of a pattern.
-- e.g. intervals [(25,1), (27,2), (25,2.5)] = [2, -2]
intervals :: Pattern -> [Interval]
intervals = fmap (uncurry (-)) . pairs . pitch

-- | The (real) rhythmic structure of a pattern.
-- e.g. durations [(25,1), (27,2), (25,2.5)] = [1, 2, 2.5]
durations :: Pattern -> [Time]
durations = fmap ontime

-- | The (relative) rhythmic structure of a pattern.
-- e.g. rhythm [(25,1), (27,2), (25,2.5)] = [1, 1.5]
rhythm :: Pattern -> [Time]
rhythm = fmap (uncurry (-)) . pairs . durations

-- | Normalized (relative) rhythmic structure of a pattern.
-- e.g. normalRhythm [(A,2), (C#,6), (Eb,8), (B,1), (A,2)] = [1, 3, 4, 1/2, 1]
normalRhythm :: Pattern -> [Time]
normalRhythm = normalizeTime . rhythm
  where
    -- | Convert times to ratios wrt the first time unit used.
    -- e.g. normalizeTime [2, 6, 8, 6, 1, 2] = [1, 3, 4, 1/2, 1]
    normalizeTime :: [Time] -> [Time]
    normalizeTime (tt : ts)  = 1 : ((/ tt) <$> ts)
    normalizeTime []         = []

-- | Translate a note horizontally (in time).
-- e.g. translateH (-0.5) [(25,1), (27,2), (25,2.5)] = [(25,0.5), (27,1.5), (25,2)]
translateH :: Time -> Note -> Note
translateH dt (Note tInit m) = Note (tInit + dt) m

-- | Translate a note vertically (in pitch).
-- e.g. translateV (-20) [(25,1), (27,2), (25,2.5)] = [(5,1), (7,2), (5,2.5)]
translateV :: Interval -> Note -> Note
translateV dm (Note tt mInit) = Note tt (mInit + dm)

-- | Get list as pairs of consecutive elements.
-- e.g. pairs [a, b, c, d] = [(a, b), (b, c), (c, d)]
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-----------------------
-- Scales/modes

type Octave    = Integer
type Degree    = Integer
type ScaleType = [Interval]
type Scale     = M.Map MIDI (Degree, Octave)

major, harmonicMinor, melodicMinor :: ScaleType
major         = [0,2,4,5,7,9,11]
melodicMinor  = [0,2,3,5,7,9,11]
harmonicMinor = [0,2,3,5,7,8,11]

createScaleInC :: ScaleType -> Scale
createScaleInC scType = M.fromList [ (24 + (oct * 12) + m, (i, oct + 1))
                                   | oct <- [0..7]
                                   , (i, m) <- zip [1..7] scType ]

allScales :: [Scale]
allScales = [ M.mapKeys (+ transp) (createScaleInC scType)
            | scType <- [major, harmonicMinor, melodicMinor]
            , transp <- [0..11] ]

guessScale :: Pattern -> Scale
guessScale xs =
  let scales = [ (sc, S.size $ M.keysSet sc `S.intersection` S.fromList (pitch xs))
               | sc <- allScales ]
  in fst $ maximumBy (\(_,s1) (_,s2) -> if s1 > s2 then GT
                                                   else if s1 < s2 then LT
                                                   else EQ) scales


toDegree :: Scale -> MIDI -> Integer
toDegree sc m = i + (oct * 7)
  where (i, oct) = M.findWithDefault (0, 0) m sc -- 0 for 'outside' note 

applyScale :: Scale -> Pattern -> [Interval]
applyScale sc = fmap (uncurry (-)) . pairs . fmap (toDegree sc) . pitch
    
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
