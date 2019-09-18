{-# LANGUAGE DeriveGeneric, FlexibleInstances, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module CompoAnalysis where

import Data.List (foldl')
import Text.Printf (printf)

import GHC.Generics (Generic)
import Data.Csv

import Types
import Transformations

-- | Analyzing a pattern group with a pattern prototype (occ1.csv).
data AnalysisResult = An
  { name           :: !String -- ^ name associated with the analysis
  , total          :: !Int -- ^ all other occurences of the pattern

  -- 100%
  , exact          :: !Int -- ^ # of exact occurences
  , transposed     :: !Int -- ^ # of *atonal* transpositions
  , tonalTransped  :: !Int -- ^ # of *tonal* transpositions
  , inverted       :: !Int -- ^ # of inversions
  , augmented      :: !Int -- ^ # of augmentations
  , retrograded    :: !Int -- ^ # of retrogrades
  , rotated        :: !Int -- ^ # of rotations
  , trInverted     :: !Int -- ^ # of transposed inversions
  , trAugmented    :: !Int -- ^ # of transposed augmentations
  , trRetrograded  :: !Int -- ^ # of transposed retrogrades

  , trRotated      :: !Int -- ^ # of transposed ratations

  -- , trtonInterted  :: !Int -- ^ # of tonal transposed inversions
  , trtonAugmented  :: !Int -- ^ # of tonal transposed augmentations
  -- , trtonRetrograded  :: !Int -- ^ # of tonal transposed retrogrades
  , trtonRotated  :: !Int -- ^ # of tonal transposed rotations

  , unclassified  :: ![(String, Pattern)] -- ^ filenames of all unclassified patterns
  }
  deriving (Generic)


instance ToNamedRecord AnalysisResult
instance DefaultOrdered AnalysisResult

instance ToField [(String, Pattern)] where
  toField = toField . length


defAn :: AnalysisResult
defAn = An "" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []


singleAn :: AnalysisResult
singleAn = defAn {total = 1}


-- | Analyze a single pattern group.
analysePatternGroup :: PatternGroup -> IO AnalysisResult
analysePatternGroup pg@(PatternGroup _ _ _ base pats) =
  returnAnalyses $ map check (zip [2..] pats)
  where
    returnAnalyses res = return (combineAnalyses res) { name = show pg }

    -- Check which equivalence class a pattern belongs to.
    check (i, p)
      | (base <=> p) (exactOf ~~ 1)            = singleAn { exact = 1 }
      | (base <=> p) (transpositionOf ~~ 1)    = singleAn { transposed = 1 }
      | (base <=> p) (tonalTranspOf ~~ 1)      = singleAn { tonalTransped = 1 }
      | (base <=> p) (inversionOf ~~ 1)        = singleAn { inverted = 1 }
      | (base <=> p) (augmentationOf ~~ 1)     = singleAn { augmented = 1 }
      | (base <=> p) (retrogradeOf ~~ 1)       = singleAn { retrograded = 1 }
      | (base <=> p) (rotationOf ~~ 1)         = singleAn { rotated = 1 }
      | (base <=> p) (trInversionOf ~~ 1)      = singleAn { trInverted = 1 }
      | (base <=> p) (trAugmentationOf ~~ 1)   = singleAn { trAugmented = 1 }
      | (base <=> p) (trRetrogradeOf ~~ 1)     = singleAn { trRetrograded = 1 }

      -- | (base <=> p) (trtonInversionOf ~~ 1)      = singleAn { trtonInverted = 1 }
      | (base <=> p) (trtonAugmentationOf ~~ 1)   = singleAn { trtonAugmented = 1 }
      -- | (base <=> p) (trtonRetrogradeOf ~~ 1)     = singleAn { trtonRetrograded = 1 }
      | (base <=> p) (trtonRotationOf ~~ 1)      = singleAn { trtonRotated = 1 }
      | otherwise
      = defAn {total = 1, unclassified = [(show pg ++ ":" ++ show i, p)]}


-- | Combine analyses from different pattern groups.
combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl' (<+>) defAn
  where
    An _ tot
       m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 xs
      <+> An _ tot' n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 ys
      = An "" (tot+tot') (m1+n1) (m2+n2) (m3+n3) (m4+n4) (m5+n5) (m6+n6) (m7+n7) (m8+n8) (m9+n9) (m10+n10)
              (m11+n11) (m12+n12) (m13+n13) (xs++ys)


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
              ++ "\n\ttonT: %.2f%% (%d)"
              ++ "\n\tinverted: %.2f%% (%d)"
              ++ "\n\taugmented: %.2f%% (%d)"
              ++ "\n\tretrograded: %.2f%% (%d)"
              ++ "\n\trotated: %.2f%% (%d)"
              ++ "\n\ttrI: %.2f%% (%d)"
              ++ "\n\ttrA: %.2f%% (%d)"
              ++ "\n\ttrR: %.2f%% (%d)"

              ++ "\n\ttonrotated: %.2f%% (%d)"
              -- ++ "\n\ttrtonI: %.2f%% (%d)"
              ++ "\n\ttrtonA: %.2f%% (%d)"
              -- ++ "\n\ttrtonR: %.2f%% (%d)"

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

           (percentage an trtonRotated) (trtonRotated an)
           -- (percentage an trtonInverted) (trtonInverted an)
           (percentage an trtonAugmented) (trtonAugmented an)
           -- (percentage an trtonRetrograded) (trtonRetrograded an)

           (otherPercentage an) (length $ unclassified an)
