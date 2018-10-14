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
  { name           :: String -- ^ name associated with the analysis
  , total          :: Int -- ^ all other occurences of the pattern
  , exact          :: Int -- ^ # of exact occurences
  , exact9         :: Int -- ^ # of approximate exact occurences (90%)
  , exact7         :: Int -- ^ # of approximate exact occurences (70%)
  , exact5         :: Int -- ^ # of approximate exact occurences (50%)
  , transposed     :: Int -- ^ # of *atonal* transpositions
  , transposed9    :: Int -- ^ # of approximate *atonal* transpositions (90%)
  , transposed7    :: Int -- ^ # of approximate *atonal* transpositions (70%)
  , transposed5    :: Int -- ^ # of approximate *atonal* transpositions (50%)
  , tonalTransped  :: Int -- ^ # of *tonal* transpositions
  , tonalTransped9 :: Int -- ^ # of approximate *tonal* transpositions (90%)
  , tonalTransped7 :: Int -- ^ # of approximate *tonal* transpositions (70%)
  , tonalTransped5 :: Int -- ^ # of approximate *tonal* transpositions (50%)
  , inverted       :: Int -- ^ # of inversions
  , augmented      :: Int -- ^ # of augmentations
  , retrograded    :: Int -- ^ # of retrogrades
  , rotated        :: Int -- ^ # of rotations
  , trInverted     :: Int -- ^ # of transposed inversions
  , trAugmented    :: Int -- ^ # of transposed augmentations
  , trRetrograded  :: Int -- ^ # of transposed retrogrades

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
      | (base <=> p) (exactOf 1)           = An "" 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (exactOf 0.9)         = An "" 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (exactOf 0.7)         = An "" 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (exactOf 0.5)         = An "" 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (transpositionOf 1)   = An "" 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (transpositionOf 0.9) = An "" 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (transpositionOf 0.7) = An "" 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (transpositionOf 0.5) = An "" 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (tonalTranspOf 1)     = An "" 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (tonalTranspOf 0.9)   = An "" 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (tonalTranspOf 0.7)   = An "" 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 []
      | (base <=> p) (tonalTranspOf 0.5)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 []
      | (base <=> p) (inversionOf 1)       = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 []
      | (base <=> p) augmentationOf        = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 []
      | (base <=> p) retrogradeOf          = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 []
      | (base <=> p) rotationOf            = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 []
      | (base <=> p) trInversionOf         = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 []
      | (base <=> p) trAugmentationOf      = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 []
      | (base <=> p) trRetrogradeOf        = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 []
      | otherwise                          = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [(getTitle pg ++ ":" ++ show i, p)]

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
              ++ "\n\texact9: %.2f%% (%d)"
              ++ "\n\texact7: %.2f%% (%d)"
              ++ "\n\texact5: %.2f%% (%d)"
              ++ "\n\ttransposed: %.2f%% (%d)"
              ++ "\n\ttransposed9: %.2f%% (%d)"
              ++ "\n\ttransposed7: %.2f%% (%d)"
              ++ "\n\ttransposed5: %.2f%% (%d)"
              ++ "\n\ttonalTransped: %.2f%% (%d)"
              ++ "\n\ttonalTransped9: %.2f%% (%d)"
              ++ "\n\ttonalTransped7: %.2f%% (%d)"
              ++ "\n\ttonalTransped5: %.2f%% (%d)"
              ++ "\n\tinverted: %.2f%% (%d)"
              ++ "\n\taugmented: %.2f%% (%d)"
              ++ "\n\tretrograded: %.2f%% (%d)"
              ++ "\n\trotated: %.2f%% (%d)"
              ++ "\n\ttrInverted: %.2f%% (%d)"
              ++ "\n\ttrAugmented: %.2f%% (%d)"
              ++ "\n\ttrRetrograded: %.2f%% (%d)"
              ++ "\n\tother: %.2f%% (%d)"
              ++ "\n}")
           (name an) (total an)
           (percentage an exact) (exact an)
           (percentage an exact5) (exact9 an)
           (percentage an exact7) (exact7 an)
           (percentage an exact9) (exact5 an)
           (percentage an transposed) (transposed an)
           (percentage an transposed9) (transposed9 an)
           (percentage an transposed7) (transposed7 an)
           (percentage an transposed5) (transposed5 an)
           (percentage an tonalTransped) (tonalTransped an)
           (percentage an tonalTransped9) (tonalTransped9 an)
           (percentage an tonalTransped7) (tonalTransped7 an)
           (percentage an tonalTransped5) (tonalTransped5 an)
           (percentage an inverted) (inverted an)
           (percentage an augmented) (augmented an)
           (percentage an retrograded) (retrograded an)
           (percentage an rotated) (rotated an)
           (percentage an trInverted) (trInverted an)
           (percentage an trAugmented) (trAugmented an)
           (percentage an trRetrograded) (trRetrograded an)
           (otherPercentage an) (length $ unclassified an)

-- | Get a title, unique to the given PatternGroup, in the following format:
-- <piece>:<expert>:<pattern>.
getTitle :: PatternGroup -> String
getTitle (PatternGroup piece_n expert_n pattern_n _ _) =
  concat $ intersperse ":" [piece_n, expert_n, pattern_n]
