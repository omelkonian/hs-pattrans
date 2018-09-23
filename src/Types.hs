module Types where

import Data.Semigroup
import Data.Functor.Contravariant hiding ((>$<), (>$), ($<))

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

---------------------
-- Combinator DSL

newtype Check a = Check { getCheck :: a -> a -> Bool }
  -- ^ Checks two patterns (horizontal translation in time is always assumed).

(<=>) :: a -> a -> Check a -> Bool
(x <=> y) p = getCheck p x y

instance Semigroup (Check a) where
  p <> q = Check $ \x y -> (x <=> y) p && (x <=> y) q

instance Contravariant Check where
  contramap f p = Check $ \x y -> (f x <=> f y) p

infix 7 >$<
(>$<) :: (a -> b) -> Check b -> Check a
(>$<) = contramap

infix 8 >$, $<
(>$), ($<) :: (a -> a) -> Check a -> Check a
f >$ p = Check $ \x y -> (f x <=> y) p
f $< p = Check $ \x y -> (x <=> f y) p

---------------------
-- Transformations

-- | Exact repetition: move a pattern in time.
-- (AKA horizontal translation)
exactOf :: Check Pattern
exactOf = rhythm >$< equal
       <> pitch  >$< equal

-- | Transposition: move a pattern in pitch.
-- (AKA horizontal+vertical translation)
transpositionOf :: Check Pattern
transpositionOf = rhythm    >$< equal
               <> intervals >$< equal

-- | Inversion: negate all pitch intervals (starting from the same base pitch).
inversionOf :: Check Pattern
inversionOf = basePitch >$< equal
           <> rhythm    >$< equal
           <> intervals >$< (inverse $< equal)

-- | Retrograde: mirror a pattern in both pitch and rhythm.
-- (AKA vertical reflection)
retrogradeOf :: Check Pattern
retrogradeOf = rhythm >$< (reverse $< equal)
            <> pitch  >$< (reverse $< equal)

-- | Rotation: reversal of pitch intervals and reversal of rhythm.
-- (AKA retrograde inversion)
rotationOf :: Check Pattern
rotationOf = rhythm    >$< (reverse $< equal)
          <> intervals >$< (reverse $< equal)

-- | Augmentation: speed-up/slow-down the rhythmic structure of a pattern.
augmentationOf :: Check Pattern
augmentationOf = normalRhythm >$< equal
              <> pitch        >$< equal

-----------------------
-- Combinations

-- | Transposition + Inversion.
-- (AKA horizontal reflection)
trInversionOf :: Check Pattern
trInversionOf = rhythm    >$< equal
             <> intervals >$< (inverse $< equal)

-- | Transposition + Augmentation.
trAugmentationOf :: Check Pattern
trAugmentationOf = normalRhythm >$< equal
                <> intervals    >$< equal

-- | Transposition + Retrograde.
-- (AKA vertical glide reflection)
trRetrogradeOf :: Check Pattern
trRetrogradeOf = rhythm    >$< (reverse $< equal)
              <> intervals >$< (reverse . inverse $< equal)


-----------------------
-- Utilities

type Interval = Integer

-- | Check that two elements are exactly equal (using `eq`).
-- e.g. [a, c, b] equal [a, c, b]
equal :: Eq a => Check a
equal = Check (==)

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
intervals = fmap (\(Note _ m, Note _ m') -> m' - m) . pairs

-- | The (real) rhythmic structure of a pattern.
-- e.g. durations [(25,1), (27,2), (25,2.5)] = [1, 2, 2.5]
durations :: Pattern -> [Time]
durations = fmap ontime

-- | The (relative) rhythmic structure of a pattern.
-- e.g. rhythm [(25,1), (27,2), (25,2.5)] = [1, 1.5]
rhythm :: Pattern -> [Time]
rhythm = fmap (\(Note t1 _, Note t2 _) -> t2 - t1) . pairs

-- | Convert times to ratios wrt the first time unit used.
-- e.g. normalizeTime [2, 6, 8, 6, 1, 2] = [1, 3, 4, 1/2, 1]
normalizeTime :: [Time] -> [Time]
normalizeTime (tt : ts)  = 1 : ((/ tt) <$> ts)
normalizeTime []         = []

-- | Normalized (relative) rhythmic structure of a pattern.
-- e.g. normalRhythm [(A,2), (C#,6), (Eb,8), (B,1), (A,2)] = [1, 3, 4, 1/2, 1]
normalRhythm :: Pattern -> [Time]
normalRhythm = normalizeTime . rhythm

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
