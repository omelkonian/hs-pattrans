{-# LANGUAGE DeriveGeneric, FlexibleInstances, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Analysis where

import qualified System.ProgressBar as P

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

  -- 80%
  , exact8         :: !Int -- ^ # of exact occurences
  , transposed8    :: !Int -- ^ # of *atonal* transpositions
  , tonalTransped8 :: !Int -- ^ # of *tonal* transpositions
  , inverted8      :: !Int -- ^ # of inversions
  , augmented8     :: !Int -- ^ # of augmentations
  , retrograded8   :: !Int -- ^ # of retrogrades
  , rotated8       :: !Int -- ^ # of rotations
  , trInverted8    :: !Int -- ^ # of transposed inversions
  , trAugmented8   :: !Int -- ^ # of transposed augmentations
  , trRetrograded8 :: !Int -- ^ # of transposed retrogrades

  -- 60%
  , exact6         :: !Int -- ^ # of exact occurences
  , transposed6    :: !Int -- ^ # of *atonal* transpositions
  , tonalTransped6 :: !Int -- ^ # of *tonal* transpositions
  , inverted6      :: !Int -- ^ # of inversions
  , augmented6     :: !Int -- ^ # of augmentations
  , retrograded6   :: !Int -- ^ # of retrogrades
  , rotated6       :: !Int -- ^ # of rotations
  , trInverted6    :: !Int -- ^ # of transposed inversions
  , trAugmented6   :: !Int -- ^ # of transposed augmentations
  , trRetrograded6 :: !Int -- ^ # of transposed retrogrades

  , unclassified  :: ![(String, Pattern)] -- ^ filenames of all unclassified patterns
  }
  deriving (Generic)

instance ToNamedRecord AnalysisResult
instance DefaultOrdered AnalysisResult
instance ToField [(String, Pattern)] where
  toField = toField . length

-- | Analyze a single pattern group.
analysePatternGroup :: Bool -> PatternGroup -> IO AnalysisResult
analysePatternGroup showProgress pg@(PatternGroup _ _ _ base pats)
  | showProgress
  = do  h <- fst <$> P.startProgress P.percentage P.exact (13 +  60) (toInteger $ length pats)
        res <- pforM (zip [2..] pats) $ \p -> do let !res = check p
                                                 P.incProgress h 1
                                                 return res
        return (combineAnalyses res) { name = show pg }
  | otherwise
  = do let res = pmap check (zip [2..] pats)
       return (combineAnalyses $ res) { name = show pg }

  where
    -- Check which equivalence class a pattern belongs to.
    check (i, p)
      | (base <=> p) (exactOf ~~ 1)            = An "" 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (transpositionOf ~~ 1)    = An "" 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (tonalTranspOf ~~ 1)      = An "" 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (inversionOf ~~ 1)        = An "" 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (augmentationOf ~~ 1)     = An "" 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (retrogradeOf ~~ 1)       = An "" 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (rotationOf ~~ 1)         = An "" 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (trInversionOf ~~ 1)      = An "" 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (trAugmentationOf ~~ 1)   = An "" 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (trRetrogradeOf ~~ 1)     = An "" 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (exactOf ~~ 0.8)          = An "" 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (transpositionOf ~~ 0.8)  = An "" 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (tonalTranspOf ~~ 0.8)    = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (inversionOf ~~ 0.8)      = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (augmentationOf ~~ 0.8)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (retrogradeOf ~~ 0.8)     = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (rotationOf ~~ 0.8)       = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (trInversionOf ~~ 0.8)    = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (trAugmentationOf ~~ 0.8) = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (trRetrogradeOf ~~ 0.8)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (exactOf ~~ 0.6)          = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 []
      | (base <=> p) (transpositionOf ~~ 0.6)  = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 []
      | (base <=> p) (tonalTranspOf ~~ 0.6)    = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 []
      | (base <=> p) (inversionOf ~~ 0.6)      = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 []
      | (base <=> p) (augmentationOf ~~ 0.6)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 []
      | (base <=> p) (retrogradeOf ~~ 0.6)     = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 []
      | (base <=> p) (rotationOf ~~ 0.6)       = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 []
      | (base <=> p) (trInversionOf ~~ 0.6)    = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 []
      | (base <=> p) (trAugmentationOf ~~ 0.6) = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 []
      | (base <=> p) (trRetrogradeOf ~~ 0.6)   = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 []
      | otherwise                              = An "" 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [(show pg ++ ":" ++ show i, p)]

-- | Combine analyses from different pattern groups.
combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl' (<+>) (An "" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [])
  where
    An _ tot m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20
       m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 xs
      <+> An _ tot' n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 n17 n18 n19 n20
             n21 n22 n23 n24 n25 n26 n27 n28 n29 n30 ys
      = An "" (tot+tot') (m1+n1) (m2+n2) (m3+n3) (m4+n4) (m5+n5) (m6+n6) (m7+n7) (m8+n8) (m9+n9) (m10+n10)
              (m11+n11) (m12+n12) (m13+n13) (m14+n14) (m15+n15) (m16+n16) (m17+n17) (m18+n18) (m19+n19) (m20+n20)
              (m21+n21) (m22+n22) (m23+n23) (m24+n24) (m25+n25) (m26+n26) (m27+n27) (m28+n28) (m29+n29) (m30+n30)
              (xs++ys)

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
              ++ "\n\texact8: %.2f%% (%d)"
              ++ "\n\ttransposed8: %.2f%% (%d)"
              ++ "\n\ttonalTransped8: %.2f%% (%d)"
              ++ "\n\tinverted8: %.2f%% (%d)"
              ++ "\n\taugmented8: %.2f%% (%d)"
              ++ "\n\tretrograded8: %.2f%% (%d)"
              ++ "\n\trotated8: %.2f%% (%d)"
              ++ "\n\ttrInverted8: %.2f%% (%d)"
              ++ "\n\ttrAugmented8: %.2f%% (%d)"
              ++ "\n\ttrRetrograded8: %.2f%% (%d)"
              ++ "\n\texact6: %.2f%% (%d)"
              ++ "\n\ttransposed6: %.2f%% (%d)"
              ++ "\n\ttonalTransped6: %.2f%% (%d)"
              ++ "\n\tinverted6: %.2f%% (%d)"
              ++ "\n\taugmented6: %.2f%% (%d)"
              ++ "\n\tretrograded6: %.2f%% (%d)"
              ++ "\n\trotated6: %.2f%% (%d)"
              ++ "\n\ttrInverted6: %.2f%% (%d)"
              ++ "\n\ttrAugmented6: %.2f%% (%d)"
              ++ "\n\ttrRetrograded6: %.2f%% (%d)"
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
           (percentage an exact8) (exact8 an)
           (percentage an transposed8) (transposed8 an)
           (percentage an tonalTransped8) (tonalTransped8 an)
           (percentage an inverted8) (inverted8 an)
           (percentage an augmented8) (augmented8 an)
           (percentage an retrograded8) (retrograded8 an)
           (percentage an rotated8) (rotated8 an)
           (percentage an trInverted8) (trInverted8 an)
           (percentage an trAugmented8) (trAugmented8 an)
           (percentage an trRetrograded8) (trRetrograded8 an)
           (percentage an exact6) (exact6 an)
           (percentage an transposed6) (transposed6 an)
           (percentage an tonalTransped6) (tonalTransped6 an)
           (percentage an inverted6) (inverted6 an)
           (percentage an augmented6) (augmented6 an)
           (percentage an retrograded6) (retrograded6 an)
           (percentage an rotated6) (rotated6 an)
           (percentage an trInverted6) (trInverted6 an)
           (percentage an trAugmented6) (trAugmented6 an)
           (percentage an trRetrograded6) (trRetrograded6 an)
           (otherPercentage an) (length $ unclassified an)
