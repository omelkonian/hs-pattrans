module Analysis where

import Data.List (intersperse)
import Text.Printf (printf)

import Types
import Transformations

-- | Analyzing a pattern group with a pattern prototype (occ1.csv).
data AnalysisResult = An
  { total         :: Int -- ^ all other occurences of the pattern
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
  , approxEq2     :: Int -- ^ # of approximately exact occurences (dist = 2)
  , approxEq4     :: Int -- ^ # of approximately exact occurences (dist = 4)
  , approxEq6     :: Int -- ^ # of approximately exact occurences (dist = 6)
  , approxEq8     :: Int -- ^ # of approximately exact occurences (dist = 8)
  , approxEq10    :: Int -- ^ # of approximately exact occurences (dist = 10)
  , approxEq12    :: Int -- ^ # of approximately exact occurences (dist = 12)
  , approxEq14    :: Int -- ^ # of approximately exact occurences (dist = 14)
  , approxEq16    :: Int -- ^ # of approximately exact occurences (dist = 16)
  }

-- | Analyze a single pattern group
analysePatternGroup :: PatternGroup -> AnalysisResult
analysePatternGroup (PatternGroup _ _ _ base pats) =
  combineAnalyses (check <$> pats)
  where
    -- Check which equivalence class a pattern belongs to.
    check p
      | (base <=> p) exactOf          = An 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) transpositionOf  = An 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) tonalTranspOf    = An 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) inversionOf      = An 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) augmentationOf   = An 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) retrogradeOf     = An 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) rotationOf       = An 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) trInversionOf    = An 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0
      | (base <=> p) trAugmentationOf = An 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0
      | (base <=> p) trRetrogradeOf   = An 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
      | (base <=> p) (approxEq 2)     = An 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0
      | (base <=> p) (approxEq 4)     = An 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0
      | (base <=> p) (approxEq 6)     = An 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0
      | (base <=> p) (approxEq 8)     = An 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
      | (base <=> p) (approxEq 10)    = An 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
      | (base <=> p) (approxEq 12)    = An 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
      | (base <=> p) (approxEq 14)    = An 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0
      | (base <=> p) (approxEq 16)    = An 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
      | otherwise                     = An 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Combine analyses from different pattern groups.
combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl (<+>) (An 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  where
    An n e tt ttt i a r ro ti ta tr e1 e2 e3 e4 e5 e6 e7 e8
      <+> An n' e' tt' ttt' i' a' r' ro' ti' ta' tr' e1' e2' e3' e4' e5' e6' e7' e8'
      = An (n+n') (e+e') (tt+tt') (ttt+ttt') (i+i') (a+a') (r+r') (ro+ro') (ti+ti')
           (ta+ta') (tr+tr') (e1+e1') (e2+e2') (e3+e3') (e4+e4') (e5+e5') (e6+e6')
           (e7+e7') (e8+e8')

-- | Get the percentage of an equivalence class from an analysis result.
percentage :: AnalysisResult -> (AnalysisResult -> Int) -> Double
percentage an f = fromIntegral (f an) / fromIntegral (total an) * 100

-- | Pretty-print analysis.
instance Show AnalysisResult where
  show an =
    printf ("{  \n\ttotal: %d"
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
            ++ "\n\tapproxEq2: %.2f%% (%d)"
            ++ "\n\tapproxEq4: %.2f%% (%d)"
            ++ "\n\tapproxEq6: %.2f%% (%d)"
            ++ "\n\tapproxEq8: %.2f%% (%d)"
            ++ "\n\tapproxEq10: %.2f%% (%d)"
            ++ "\n\tapproxEq12: %.2f%% (%d)"
            ++ "\n\tapproxEq14: %.2f%% (%d)"
            ++ "\n\tapproxEq16: %.2f%% (%d)"
            ++ "\n}")
           (total an)
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
           (percentage an approxEq2) (approxEq2 an)
           (percentage an approxEq4) (approxEq4 an)
           (percentage an approxEq6) (approxEq6 an)
           (percentage an approxEq8) (approxEq8 an)
           (percentage an approxEq10) (approxEq10 an)
           (percentage an approxEq12) (approxEq12 an)
           (percentage an approxEq14) (approxEq14 an)
           (percentage an approxEq16) (approxEq16 an)

-- | Get a title, unique to the given PatternGroup, in the following format:
-- <piece>:<expert>:<pattern>.
getTitle :: PatternGroup -> String
getTitle (PatternGroup piece_n expert_n pattern_n _ _) =
  concat $ intersperse ":" [piece_n, expert_n, pattern_n]
