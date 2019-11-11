{-# LANGUAGE ImplicitParams, Rank2Types, ScopedTypeVariables, BangPatterns #-}
module Transformations where

import Data.List (sortOn)
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
  p <> q = Check $ \x y -> and (x <=> y `map` [p, q])

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

-- | Octave-agnostic tonal transposition, wrt a scale that 'fits' the base pattern
-- e.g. [I, IV, V] tonalTranspOf [III, VI, VII]
tonalTranspOf :: ApproxCheck Pattern
tonalTranspOf =  rhythm >$< approxEq2
              <> Check (\xs ys -> (xs <=> ys) (applyScale (guessScale $ xs ++ ys)
                                               >$< (intervals >$< approxEq2)))

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


-- | New tonal versions
trtonRotationOf :: ApproxCheck Pattern
trtonRotationOf = rhythm >$< (reverse $< approxEq2)
                   <> Check (\xs ys -> (xs <=> ys) (applyScale (guessScale xs)
                                               >$< (intervals >$< approxEq2)))

trtonAugmentationOf :: ApproxCheck Pattern
trtonAugmentationOf = normalRhythm >$< approxEq2
                   <> Check (\xs ys -> (xs <=> ys) (applyScale (guessScale xs)
                                               >$< (intervals >$< approxEq2)))


-----------------------
-- Approximate equality

-- | Check that two elements are exactly equal (using `eq`).
-- e.g. [a, c, b] equal [a, c, b]
equal :: Eq a => Check a
equal = Check (==)

-- | Influences the accuracy of `approxEq`, but also dramatically reduces
-- execution time of the analysis.
maxLookahead :: Int
maxLookahead = 5

approxEqWith :: forall b. (Show b, Num b, Eq b)
             => (  b                -- ^ the element to delete
                -> [b]              -- ^ the initial list
                -> Int              -- ^ maximum elements to ignore
                -> Maybe (Int, [b]) -- ^ * Nothing, if there was no deletion
                                    --   * Just(# of ignored,tail), otherwise
                )
                -- ^ function that deletes an element from a list, possibly
                -- reducing (summing) consecutive elements to be equal to the
                -- element being deleted
             -> ApproxCheck [b]
approxEqWith del1
  | ?p == 1.0 = equal -- short-circuit for faster results
  | otherwise = Check go
  where
    go xs' ys' =
      let [xs, ys]   = sortOn length [xs', ys']
          [n, m]     = length <$> [xs, ys]
          maxIgnored = floor $ (1 - ?p) * fromIntegral n
          maxAdded   = floor $ (1 - ?p) * fromIntegral m
      in  del ys xs (maxIgnored, maxAdded)

    del :: [b] -> [b] -> (Int {-ignored-}, Int {-added-}) -> Bool
    del ys []     (maxI, maxA) = 0         <= maxI && length ys <= maxA
    del [] xs     (maxI, maxA) = length xs <= maxI && 0         <= maxA
    del ys (x:xs) (maxI, maxA)
      -- surpassed the limits, abort
      | maxI < 0 || maxA < 0
      = False

      -- no more additions/ignores allowed, resort to simple equality
      | maxI + maxA == 0
      = ys == x:xs

      -- found the prototype element in the occurrence (possibly adding elements)
      -- NB: maybe it's better to ignore it though, thus the second case
      | Just (maxA', ys') <- del1 x ys maxA
      , maxA' >= 0
      , maxA - maxA' <= maxLookahead
      = del ys' xs (maxI, maxA')
      -- || (maxI > 0 && del ys xs (maxI - 1, maxA)) -- too slow...

      -- did not find element, ignore if possible
      | maxI > 0
      = del ys xs (maxI - 1, maxA)

      -- did not find element and cannot ignore it, abort
      | otherwise
      = False

-- | First-order approximate equality of lists.
--
-- Check that two lists are approximately equal, wrt a certain percentage.
-- A base pattern and an occurence are approximately equal with percentage `p` when:
--    1. The occurence ignores (1-p)% notes of the base pattern
--    2. (1-p)% notes of the occurence are additional notes (not in the base pattern)
-- e.g. [A,C,F,A,B] (approxEq 80%) [A,C,G,A,B]
approxEq :: (Show a, Num a, Eq a) => ApproxCheck [a]
approxEq = approxEqWith del1
  where
    -- does not reduce consecutive elements (first-order)
    del1 _ []     _    = Nothing
    del1 x (y:ys) maxA
      | maxA < 0  = Nothing
      | x == y    = Just (maxA, ys)
      | otherwise = del1 x ys $! (maxA - 1)

-- | Second-order approximate equality of lists.
--
-- The essential difference with first-order approximate equality is the ability
-- to equate consecutive elements with their sum, hence the Ord/Num constraint.
--
-- NB: motivated by lists which are the result of pairing an initial list
-- and we count approximation by checking the initial lists
-- e.g. * intervals from pitches
--      * rhythm from durations
approxEq2 :: (Show a, Ord a, Num a, Eq a) => ApproxCheck [a]
approxEq2 = approxEqWith del1
  where
    -- reduces consecutive elements (second-order)
    del1 _ []     _    = Nothing
    del1 x (y:ys) maxA
      | maxA < 0
      = Nothing
      | x == y
      = Just (maxA, ys)
      | Just i <- findIndex 0 x (y:ys) maxLookahead -- NB: fixed look-ahead
      , maxA >= i
      = Just (maxA - i, snd $ splitAt i ys)
      | otherwise
      = del1 x ys $! (maxA - 1)

    findIndex i 0   _      _ = Just i
    findIndex _ _   _      0 = Nothing
    findIndex _ _   []     _ = Nothing
    findIndex i acc (y:ys) maxAc
      | acc >= y  = findIndex (i + 1) (acc - y) ys (maxAc - 1)
      | otherwise = Nothing
