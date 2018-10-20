{-# LANGUAGE GADTs, ImplicitParams, Rank2Types #-}
module Transformations where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (maximumBy, sortOn)
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
  p <> q = Check $ \x y -> and (x <=> y `pmap` [p, q])

instance Contravariant Check where
  contramap f p = Check $ \x y -> (f x <=> f y) p

infix 7 >$<
(>$<) :: (a -> b) -> Check b -> Check a
(>$<) = contramap

infix 8 >$, $<
(>$), ($<) :: (a -> a) -> Check a -> Check a
f >$ p = Check $ \x y -> (f x <=> y) p
f $< p = Check $ \x y -> (x <=> f y) p

type ApproxCheck a = (?p :: Float) => Check a

(~~) :: ((?p::Float) => r) -> Float -> r
(~~) thing f = let ?p = f in thing

---------------------
-- Transformations

-- | Exact repetition: move a pattern in time.
-- (AKA horizontal translation)
exactOf :: ApproxCheck Pattern
exactOf = rhythm >$< approxEq2
       <> pitch  >$< approxEq

-- | Transposition: move a pattern in pitch.
-- (AKA horizontal+vertical translation)
transpositionOf :: ApproxCheck Pattern
transpositionOf = rhythm    >$< approxEq2
               <> intervals >$< approxEq2

-- | Inversion: negate all pitch intervals (starting from the same base pitch).
inversionOf :: ApproxCheck Pattern
inversionOf = basePitch >$< equal
           <> rhythm    >$< approxEq2
           <> intervals >$< (inverse $< approxEq2)

-- | Retrograde: mirror a pattern in both pitch and rhythm.
-- (AKA vertical reflection)
retrogradeOf :: ApproxCheck Pattern
retrogradeOf = rhythm >$< (reverse $< approxEq2)
            <> pitch  >$< (reverse $< approxEq)

-- | Rotation: reversal of pitch intervals and reversal of rhythm.
-- (AKA retrograde inversion)
rotationOf :: ApproxCheck Pattern
rotationOf = rhythm    >$< (reverse $< approxEq2)
          <> intervals >$< (reverse $< approxEq2)

-- | Augmentation: speed-up/slow-down the rhythmic structure of a pattern.
augmentationOf :: ApproxCheck Pattern
augmentationOf = normalRhythm >$< approxEq2
              <> pitch        >$< approxEq

-----------------------
-- Tonal transposition

type Degree = Integer
type Scale = M.Map MIDI Degree

-- | Octave-agnostic tonal transposition, wrt a scale that 'fits' the base pattern
-- e.g. [I, IV, V] tonalTranspOf [III, VI, VII]
tonalTranspOf :: ApproxCheck Pattern
tonalTranspOf =  rhythm >$< approxEq2
              <> Check (\xs ys -> (xs <=> ys) (applyScale (guessScale xs)
                                               >$< (intervals >$< approxEq2)))
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
trInversionOf :: ApproxCheck Pattern
trInversionOf = rhythm    >$< approxEq2
             <> intervals >$< (inverse $< approxEq2)

-- | Transposition + Augmentation.
trAugmentationOf :: ApproxCheck Pattern
trAugmentationOf = normalRhythm >$< approxEq2
                <> intervals    >$< approxEq2

-- | Transposition + Retrograde.
-- (AKA vertical glide reflection)
trRetrogradeOf :: ApproxCheck Pattern
trRetrogradeOf = rhythm    >$< (reverse $< approxEq2)
              <> intervals >$< (reverse . inverse $< approxEq2)

-----------------------
-- Utilities

type Interval = Integer

-- | Check that two elements are exactly equal (using `eq`).
-- e.g. [a, c, b] equal [a, c, b]
equal :: Eq a => Check a
equal = Check (==)

approxEqWith :: (Show b, Num b, Eq b, a ~ [b], ?p :: Float)
             => (b -> [b] -> Maybe [b] {-list after deleting the element-})
                -- ^ function that deletes an element from a list, possibly
                -- reducing (summing) consecutive elements to be equal to the
                -- element being deleted
             -> Check a
approxEqWith del1 = Check go
  where
    go xs' ys' =
      let [xs, ys] = sortOn length [xs', ys']
          [n, m]   = length <$> [xs, ys]
          (remained, ignored) = ys `del` xs
          toF      =  fromIntegral
      in  (toF ignored <= (1 - ?p) * toF n) && (toF remained <= (1 - ?p) * toF m)

    del ys [] = (length ys, 0)
    del [] xs = (0, length xs)
    del ys (x:xs)
      | Just ys' <- del1 x ys = del ys' xs
      | otherwise             = (+ 1) <$> del ys xs

-- | First-order approximate equality of lists.
--
-- Check that two lists are approximately equal, wrt a certain percentage.
-- A base pattern and an occurence are approximately equal with percentage `p` when:
--    1. The occurence ignores (1-p)% notes of the base pattern
--    2. (1-p)% notes of the occurence are additional notes (not in the base pattern)
-- e.g. [A,C,F,A,B] (approxEq 80%) [A,C,G,A,B]
approxEq :: (Show b, Num b, Eq b, a ~ [b], ?p :: Float) => Check a
approxEq
  | ?p == 1.0 = equal
  | otherwise = approxEqWith del1
  where
    -- does not reduce consecutive elements (first-order)
    del1 _ []     = Nothing
    del1 x (y:ys)
      | x == y    = Just ys
      | otherwise = (y:) <$> del1 x ys

-- | Second-order approximate equality of lists.
--
-- The essential difference with first-order approximate equality is the ability
-- to equate consecutive elements with their sum, hence the Ord/Num constraint.
--
-- NB: motivated by lists which are the result of pairing an initial list
-- and we count approximation by checking the initial lists
-- e.g. * intervals from pitches
--      * rhythm from durations
approxEq2 :: (Show b, Ord b, Num b, Eq b, a ~ [b], ?p :: Float) => Check a
approxEq2 = approxEqWith del1
  where
    -- reduces consecutive elements (second-order)
    del1 _ [] = Nothing
    del1 x (y:ys)
      | x == y                         = Just ys
      | Just i <- findIndex 0 x (y:ys) = Just (snd $ splitAt i ys)
      | otherwise                      = (y:) <$> del1 x ys

    findIndex i 0 _  = Just i
    findIndex _ _ [] = Nothing
    findIndex i acc (y:ys)
      | acc >= y  = findIndex (i + 1) (acc - y) ys
      | otherwise = Nothing

{- ^^^ COMPUTATIONALLY INFEASIBLE (even when threaded, due to space leaks) ^^^
approxEq2 :: (Semigroup b, Eq b, a ~ [b], ?p :: Float) => Check a
approxEq2
  | ?p == 1.0 = equal
  | otherwise = Check $ \x y ->
      or $ parMap rpar (\y' -> (x <=> y') approxEq) (ndShrink y)
  where
    -- | Non-deterministically combine consecutive elements of the array.
    ndShrink :: Semigroup a => [a] -> [[a]]
    ndShrink []     = [[]]
    ndShrink (x:xs) = ((x:) <$> rest) ++ (merge <$> rest)
      where rest = ndShrink xs
            merge []     = []
            merge (y:ys) = (x <> y) : ys
-}

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
