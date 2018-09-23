module Analysis where

import Data.List (intersperse)
import Text.Printf (printf)

import Types

-- | Analyzing a pattern group with a pattern prototype (occ1.csv).
data AnalysisResult = An
  { total         :: Int -- ^ all other occurences of the pattern
  , exact         :: Int -- ^ # of exact occurences
  , transposed    :: Int -- ^ # of transpositions
  , inverted      :: Int -- ^ # of inversions
  , augmented     :: Int -- ^ # of augmentations
  , retrograded   :: Int -- ^ # of retrogrades
  , rotated       :: Int -- ^ # of rotations
  , trInverted    :: Int -- ^ # of transposed inversions
  , trAugmented   :: Int -- ^ # of transposed augmentations
  , trRetrograded :: Int -- ^ # of transposed retrogrades
  }

-- | Analyze a single pattern group
analysePatternGroup :: PatternGroup -> AnalysisResult
analysePatternGroup (PatternGroup _ _ _ base pats) =
  combineAnalyses (check <$> pats)
  where
    -- Check which equivalence class a pattern belongs to.
    check p
      | (p <=> base) exactOf          = An 1 1 0 0 0 0 0 0 0 0
      | (p <=> base) transpositionOf  = An 1 0 1 0 0 0 0 0 0 0
      | (p <=> base) inversionOf      = An 1 0 0 1 0 0 0 0 0 0
      | (p <=> base) augmentationOf   = An 1 0 0 0 1 0 0 0 0 0
      | (p <=> base) retrogradeOf     = An 1 0 0 0 0 1 0 0 0 0
      | (p <=> base) rotationOf       = An 1 0 0 0 0 0 1 0 0 0
      | (p <=> base) trInversionOf    = An 1 0 0 0 0 0 0 1 0 0
      | (p <=> base) trAugmentationOf = An 1 0 0 0 0 0 0 0 1 0
      | (p <=> base) trRetrogradeOf   = An 1 0 0 0 0 0 0 0 0 1
      | otherwise                     = An 1 0 0 0 0 0 0 0 0 0

-- | Combine analyses from different pattern groups.
combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl (<+>) (An 0 0 0 0 0 0 0 0 0 0)
  where
    An n e tt i a r ro ti ta tr <+> An n' e' tt' i' a' r' ro' ti' ta' tr' =
      An (n+n') (e+e') (tt+tt') (i+i') (a+a') (r+r') (ro+ro') (ti+ti') (ta+ta') (tr+tr')

-- | Get the percentage of an equivalence class from an analysis result.
percentage :: AnalysisResult -> (AnalysisResult -> Int) -> Double
percentage an f = fromIntegral (f an) / fromIntegral (total an) * 100

-- | Pretty-print analysis.
instance Show AnalysisResult where
  show an =
    printf ("{  \n\ttotal: %d"
            ++ "\n\texact: %.2f%% (%d)"
            ++ "\n\ttransposed: %.2f%% (%d)"
            ++ "\n\tinverted: %.2f%% (%d)"
            ++ "\n\taugmented: %.2f%% (%d)"
            ++ "\n\tretrograded: %.2f%% (%d)"
            ++ "\n\trotated: %.2f%% (%d)"
            ++ "\n\ttrInverted: %.2f%% (%d)"
            ++ "\n\ttrAugmented: %.2f%% (%d)"
            ++ "\n\ttrRetrograded: %.2f%% (%d)"
            ++ "\n}")
           (total an)
           (percentage an exact) (exact an)
           (percentage an transposed) (transposed an)
           (percentage an inverted) (inverted an)
           (percentage an augmented) (augmented an)
           (percentage an retrograded) (retrograded an)
           (percentage an rotated) (rotated an)
           (percentage an trInverted) (trInverted an)
           (percentage an trAugmented) (trAugmented an)
           (percentage an trRetrograded) (trRetrograded an)

-- | Get a title, unique to the given PatternGroup, in the following format:
-- <piece>:<expert>:<pattern>.
getTitle :: PatternGroup -> String
getTitle (PatternGroup piece_n expert_n pattern_n _ _) =
  concat $ intersperse ":" [piece_n, expert_n, pattern_n]
