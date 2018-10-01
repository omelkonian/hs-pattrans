module Transformations where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List ((\\), maximumBy)
import Data.Semigroup
import Data.Functor.Contravariant hiding ((>$<), (>$), ($<))

import Types

--------------------
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
-- Tonal transposition

type Degree = Integer
type Scale = M.Map MIDI Degree

-- | Octave-agnostic tonal transposition, wrt a scale that 'fits' the base pattern
-- e.g. [I, IV, V] tonalTranspOf [III, VI, VII]
tonalTranspOf :: Check Pattern
tonalTranspOf = Check $ \xs ys ->
  (xs <=> ys) (applyScale (guessScale xs) >$< (intervals >$< equal))
  where
    toDegree :: MIDI -> Scale -> Degree
    toDegree = M.findWithDefault 0 -- 'outside' note

    cMajor :: Scale
    cMajor = M.fromList [ (24 + (oct * 12) + m, i)
                        | oct <- [1..7]
                        , (i, m) <- zip [1..7] [0,2,2,1,2,2,2] ]

    allScales :: [Scale]
    allScales = [ M.mapKeys (+ transp) cMajor | transp <- [0..11] ]

    guessScale :: Pattern -> Scale
    guessScale xs = fst $ maximumBy (\(_,s1) (_,s2) -> if s1 > s2 then GT
                                                       else if s1 < s2 then LT
                                                       else EQ)
                        [ ( sc
                          , S.size $ S.intersection (M.keysSet sc) (S.fromList $ pitch xs)
                          )
                        | sc <- allScales ]

    applyScale :: Scale -> Pattern -> Pattern
    applyScale sc (Note tt m : xs) = Note tt (toDegree m sc) : applyScale sc xs
    applyScale _  []               = []

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

-- | Check that two patterns are approximately equal, ie. have at most `n` differences.
-- e.g. [A, C, F] (approxEq 1) [A, C, C]
approxEq :: Int -> Check Pattern
approxEq n = Check $ \xs ys -> go xs ys <= n
  where
    go xs ys =
      let zs = ys \\ xs
          remained = length zs
          ignored = length xs - (length ys - remained)
      in  remained + ignored

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
