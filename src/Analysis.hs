{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Analysis where

import Data.List (intersperse)
import Text.Printf (printf)

import GHC.Generics (Generic)
import Data.Csv

import Types
import Transformations

-- | Analyzing a pattern group with a pattern prototype (occ1.csv).
data AnalysisResult = An
  { name          :: String -- ^ name associated with the analysis
  , total         :: Int -- ^ all other occurences of the pattern
  , exact         :: Int -- ^ # of exact occurences
  , transposed    :: Int -- ^ # of *atonal* transpositions
  , tonalTransped :: Int -- ^ # of *tonal* transpositions
  , inverted      :: Int -- ^ # of inversions
  , augmented     :: Int -- ^ # of augmentations
  , retrograded   :: Int -- ^ # of retrogrades
  , rotated       :: Int -- ^ # of rotations
  , trInverted    :: Int -- ^ # of transposed inversions
  , trAugmented   :: Int -- ^ # of transposed augmentations
  , trRetrograded :: Int -- ^ # of transposed retrogrades
  , approxEq9     :: Int -- ^ # of approximately equal occurences (90%)
  , approxEq8     :: Int -- ^ # of approximately equal occurences (80%)
  , approxEq7     :: Int -- ^ # of approximately equal occurences (70%)
  , approxEq6     :: Int -- ^ # of approximately equal occurences (60%)
  , approxEq5     :: Int -- ^ # of approximately equal occurences (50%)
  , approxEq4     :: Int -- ^ # of approximately equal occurences (40%)
  , approxEq3     :: Int -- ^ # of approximately equal occurences (30%)
  , approxEq2     :: Int -- ^ # of approximately equal occurences (20%)
  , approxEq1     :: Int -- ^ # of approximately equal occurences (10%)
  , unclassified  :: [(String, Pattern)] -- ^ filenames of all unclassified patterns
  }
  deriving (Generic)

instance ToNamedRecord AnalysisResult
instance DefaultOrdered AnalysisResult
instance ToField [(String, Pattern)] where
  toField = toField . length

-- | Analyze a single pattern group
analysePatternGroup :: PatternGroup -> AnalysisResult
analysePatternGroup pg@(PatternGroup _ _ _ base pats) =
  (combineAnalyses (check <$> zip [2..] pats)) { name = getTitle pg }
  where
    -- Check which equivalence class a pattern belongs to.
    check (i, p)
      | (base <=> p) exactOf          = An "" 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) transpositionOf  = An "" 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) tonalTranspOf    = An "" 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) inversionOf      = An "" 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) augmentationOf   = An "" 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) retrogradeOf     = An "" 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) rotationOf       = An "" 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) trInversionOf    = An "" 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) trAugmentationOf = An "" 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) trRetrogradeOf   = An "" 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (approxEq 0.9)   = An "" 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 []
      | (base <=> p) (approxEq 0.8)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 []
      | (base <=> p) (approxEq 0.7)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 []
      | (base <=> p) (approxEq 0.6)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 []
      | (base <=> p) (approxEq 0.5)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 []
      | (base <=> p) (approxEq 0.4)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 []
      | (base <=> p) (approxEq 0.3)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 []
      | (base <=> p) (approxEq 0.2)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 []
      | (base <=> p) (approxEq 0.1)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 []
      | otherwise                     = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [(getTitle pg ++ ":" ++ show i, p)]

-- | Combine analyses from different pattern groups.
combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl (<+>) (An "" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [])
  where
    An _ n e tt ttt i a r ro ti ta tr e1 e2 e3 e4 e5 e6 e7 e8 e9 xs
      <+> An _ n' e' tt' ttt' i' a' r' ro' ti' ta' tr' e1' e2' e3' e4' e5' e6' e7' e8' e9' ys
      = An "" (n+n') (e+e') (tt+tt') (ttt+ttt') (i+i') (a+a') (r+r') (ro+ro')
           (ti+ti') (ta+ta') (tr+tr') (e1+e1') (e2+e2') (e3+e3') (e4+e4') (e5+e5')
           (e6+e6') (e7+e7') (e8+e8') (e9 + e9') (xs++ys)

-- | Get the percentage of an equivalence class from an analysis result.
percentage :: AnalysisResult -> (AnalysisResult -> Int) -> Double
percentage an f = fromIntegral (f an) / fromIntegral (total an) * 100

-- | Get the percentage of unclassified patterns from an analysis result.
otherPercentage :: AnalysisResult -> Double
otherPercentage an =
  fromIntegral (length $ unclassified an) / fromIntegral (total an) * 100

-- | Pretty-print analysis.
instance Show AnalysisResult where
  show an =
    printf ("%s { \n\ttotal: %d"
              ++ "\n\texact: %.2f%% (%d)"
              ++ "\n\ttransposed: %.2f%% (%d)"
              ++ "\n\ttonalTransped: %.2f%% (%d)"
              ++ "\n\tinverted: %.2f%% (%d)"
              ++ "\n\taugmented: %.2f%% (%d)"
              ++ "\n\tretrograded: %.2f%% (%d)"
              ++ "\n\trotated: %.2f%% (%d)"
              ++ "\n\ttrInverted: %.2f%% (%d)"
              ++ "\n\ttrAugmented: %.2f%% (%d)"
              ++ "\n\ttrRetrograded: %.2f%% (%d)"
              ++ "\n\tapproxEq9: %.2f%% (%d)"
              ++ "\n\tapproxEq8: %.2f%% (%d)"
              ++ "\n\tapproxEq7: %.2f%% (%d)"
              ++ "\n\tapproxEq6: %.2f%% (%d)"
              ++ "\n\tapproxEq5: %.2f%% (%d)"
              ++ "\n\tapproxEq4: %.2f%% (%d)"
              ++ "\n\tapproxEq3: %.2f%% (%d)"
              ++ "\n\tapproxEq2: %.2f%% (%d)"
              ++ "\n\tapproxEq1: %.2f%% (%d)"
              ++ "\n\tother: %.2f%% (%d)"
              ++ "\n}")
           (name an) (total an)
           (percentage an exact) (exact an)
           (percentage an transposed) (transposed an)
           (percentage an tonalTransped) (tonalTransped an)
           (percentage an inverted) (inverted an)
           (percentage an augmented) (augmented an)
           (percentage an retrograded) (retrograded an)
           (percentage an rotated) (rotated an)
           (percentage an trInverted) (trInverted an)
           (percentage an trAugmented) (trAugmented an)
           (percentage an trRetrograded) (trRetrograded an)
           (percentage an approxEq9) (approxEq9 an)
           (percentage an approxEq8) (approxEq8 an)
           (percentage an approxEq7) (approxEq7 an)
           (percentage an approxEq6) (approxEq6 an)
           (percentage an approxEq5) (approxEq5 an)
           (percentage an approxEq4) (approxEq4 an)
           (percentage an approxEq3) (approxEq3 an)
           (percentage an approxEq2) (approxEq2 an)
           (percentage an approxEq1) (approxEq1 an)
           (otherPercentage an) (length $ unclassified an)

-- | Get a title, unique to the given PatternGroup, in the following format:
-- <piece>:<expert>:<pattern>.
getTitle :: PatternGroup -> String
getTitle (PatternGroup piece_n expert_n pattern_n _ _) =
  concat $ intersperse ":" [piece_n, expert_n, pattern_n]
