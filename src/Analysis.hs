{-# LANGUAGE DeriveGeneric, FlexibleInstances, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Analysis where

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

  -- 40%
  , exact4         :: !Int -- ^ # of exact occurences
  , transposed4    :: !Int -- ^ # of *atonal* transpositions
  , tonalTransped4 :: !Int -- ^ # of *tonal* transpositions
  , inverted4      :: !Int -- ^ # of inversions
  , augmented4     :: !Int -- ^ # of augmentations
  , retrograded4   :: !Int -- ^ # of retrogrades
  , rotated4       :: !Int -- ^ # of rotations
  , trInverted4    :: !Int -- ^ # of transposed inversions
  , trAugmented4   :: !Int -- ^ # of transposed augmentations
  , trRetrograded4 :: !Int -- ^ # of transposed retrogrades

  -- 20%
  , exact2         :: !Int -- ^ # of exact occurences
  , transposed2    :: !Int -- ^ # of *atonal* transpositions
  , tonalTransped2 :: !Int -- ^ # of *tonal* transpositions
  , inverted2      :: !Int -- ^ # of inversions
  , augmented2     :: !Int -- ^ # of augmentations
  , retrograded2   :: !Int -- ^ # of retrogrades
  , rotated2       :: !Int -- ^ # of rotations
  , trInverted2    :: !Int -- ^ # of transposed inversions
  , trAugmented2   :: !Int -- ^ # of transposed augmentations
  , trRetrograded2 :: !Int -- ^ # of transposed retrogrades

  , unclassified  :: ![(String, Pattern)] -- ^ filenames of all unclassified patterns
  }
  deriving (Generic)

instance ToNamedRecord AnalysisResult
instance DefaultOrdered AnalysisResult
instance ToField [(String, Pattern)] where
  toField = toField . length

defAn :: AnalysisResult
defAn = An "" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []

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
      | (base <=> p) (tonalTranspOfCan ~~ 1)      = singleAn { tonalTransped = 1 }
      | (base <=> p) (inversionOf ~~ 1)        = singleAn { inverted = 1 }
      | (base <=> p) (augmentationOf ~~ 1)     = singleAn { augmented = 1 }
      | (base <=> p) (retrogradeOf ~~ 1)       = singleAn { retrograded = 1 }
      | (base <=> p) (rotationOf ~~ 1)         = singleAn { rotated = 1 }
      | (base <=> p) (trInversionOf ~~ 1)      = singleAn { trInverted = 1 }
      | (base <=> p) (trAugmentationOf ~~ 1)   = singleAn { trAugmented = 1 }
      | (base <=> p) (trRetrogradeOf ~~ 1)     = singleAn { trRetrograded = 1 }
      | (base <=> p) (exactOf ~~ 0.8)          = singleAn { exact8 = 1 }
      | (base <=> p) (transpositionOf ~~ 0.8)  = singleAn { transposed8 = 1 }
      | (base <=> p) (tonalTranspOfCan ~~ 0.8)    = singleAn { tonalTransped8 = 1 }
      | (base <=> p) (inversionOf ~~ 0.8)      = singleAn { inverted8 = 1 }
      | (base <=> p) (augmentationOf ~~ 0.8)   = singleAn { augmented8 = 1 }
      | (base <=> p) (retrogradeOf ~~ 0.8)     = singleAn { retrograded8 = 1 }
      | (base <=> p) (rotationOf ~~ 0.8)       = singleAn { rotated8 = 1 }
      | (base <=> p) (trInversionOf ~~ 0.8)    = singleAn { trInverted8 = 1 }
      | (base <=> p) (trAugmentationOf ~~ 0.8) = singleAn { trAugmented8 = 1 }
      | (base <=> p) (trRetrogradeOf ~~ 0.8)   = singleAn { trRetrograded8 = 1 }
      | (base <=> p) (exactOf ~~ 0.6)          = singleAn { exact6 = 1 }
      | (base <=> p) (transpositionOf ~~ 0.6)  = singleAn { transposed6 = 1 }
      | (base <=> p) (tonalTranspOfCan ~~ 0.6)    = singleAn { tonalTransped6 = 1 }
      | (base <=> p) (inversionOf ~~ 0.6)      = singleAn { inverted6 = 1 }
      | (base <=> p) (augmentationOf ~~ 0.6)   = singleAn { augmented6 = 1 }
      | (base <=> p) (retrogradeOf ~~ 0.6)     = singleAn { retrograded6 = 1 }
      | (base <=> p) (rotationOf ~~ 0.6)       = singleAn { rotated6 = 1 }
      | (base <=> p) (trInversionOf ~~ 0.6)    = singleAn { trInverted6 = 1 }
      | (base <=> p) (trAugmentationOf ~~ 0.6) = singleAn { trAugmented6 = 1 }
      | (base <=> p) (trRetrogradeOf ~~ 0.6)   = singleAn { trRetrograded6 = 1 }
      | (base <=> p) (exactOf ~~ 0.4)          = singleAn { exact4 = 1 }
      | (base <=> p) (transpositionOf ~~ 0.4)  = singleAn { transposed4 = 1 }
      | (base <=> p) (tonalTranspOfCan ~~ 0.4)    = singleAn { tonalTransped4 = 1 }
      | (base <=> p) (inversionOf ~~ 0.4)      = singleAn { inverted4 = 1 }
      | (base <=> p) (augmentationOf ~~ 0.4)   = singleAn { augmented4 = 1 }
      | (base <=> p) (retrogradeOf ~~ 0.4)     = singleAn { retrograded4 = 1 }
      | (base <=> p) (rotationOf ~~ 0.4)       = singleAn { rotated4 = 1 }
      | (base <=> p) (trInversionOf ~~ 0.4)    = singleAn { trInverted4 = 1 }
      | (base <=> p) (trAugmentationOf ~~ 0.4) = singleAn { trAugmented4 = 1 }
      | (base <=> p) (trRetrogradeOf ~~ 0.4)   = singleAn { trRetrograded4 = 1 }
      | (base <=> p) (exactOf ~~ 0.2)          = singleAn { exact2 = 1 }
      | (base <=> p) (transpositionOf ~~ 0.2)  = singleAn { transposed2 = 1 }
      | (base <=> p) (tonalTranspOfCan ~~ 0.2)    = singleAn { tonalTransped2 = 1 }
      | (base <=> p) (inversionOf ~~ 0.2)      = singleAn { inverted2 = 1 }
      | (base <=> p) (augmentationOf ~~ 0.2)   = singleAn { augmented2 = 1 }
      | (base <=> p) (retrogradeOf ~~ 0.2)     = singleAn { retrograded2 = 1 }
      | (base <=> p) (rotationOf ~~ 0.2)       = singleAn { rotated2 = 1 }
      | (base <=> p) (trInversionOf ~~ 0.2)    = singleAn { trInverted2 = 1 }
      | (base <=> p) (trAugmentationOf ~~ 0.2) = singleAn { trAugmented2 = 1 }
      | (base <=> p) (trRetrogradeOf ~~ 0.2)   = singleAn { trRetrograded2 = 1 }
      | otherwise
      = defAn {total = 1, unclassified = [(show pg ++ ":" ++ show i, p)]}

-- | Combine analyses from different pattern groups.
combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl' (<+>) defAn
  where
    An _ tot
       m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15 m16 m17 m18 m19 m20
       m21 m22 m23 m24 m25 m26 m27 m28 m29 m30 m31 m32 m33 m34 m35 m36 m37 m38 m39 m40
       m41 m42 m43 m44 m45 m46 m47 m48 m49 m50 xs
      <+> An _ tot' n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12 n13 n14 n15 n16 n17 n18 n19 n20
             n21 n22 n23 n24 n25 n26 n27 n28 n29 n30 n31 n32 n33 n34 n35 n36 n37 n38 n39 n40
             n41 n42 n43 n44 n45 n46 n47 n48 n49 n50 ys
      = An "" (tot+tot') (m1+n1) (m2+n2) (m3+n3) (m4+n4) (m5+n5) (m6+n6) (m7+n7) (m8+n8) (m9+n9) (m10+n10)
              (m11+n11) (m12+n12) (m13+n13) (m14+n14) (m15+n15) (m16+n16) (m17+n17) (m18+n18) (m19+n19) (m20+n20)
              (m21+n21) (m22+n22) (m23+n23) (m24+n24) (m25+n25) (m26+n26) (m27+n27) (m28+n28) (m29+n29) (m30+n30)
              (m31+n31) (m32+n32) (m33+n33) (m34+n34) (m35+n35) (m36+n36) (m37+n37) (m38+n38) (m39+n39) (m40+n40)
              (m41+n41) (m42+n42) (m43+n43) (m44+n44) (m45+n45) (m46+n46) (m47+n47) (m48+n48) (m49+n49) (m50+n50)
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
              ++ "\n\texact4: %.2f%% (%d)"
              ++ "\n\ttransposed4: %.2f%% (%d)"
              ++ "\n\ttonalTransped4: %.2f%% (%d)"
              ++ "\n\tinverted4: %.2f%% (%d)"
              ++ "\n\taugmented4: %.2f%% (%d)"
              ++ "\n\tretrograded4: %.2f%% (%d)"
              ++ "\n\trotated4: %.2f%% (%d)"
              ++ "\n\ttrInverted4: %.2f%% (%d)"
              ++ "\n\ttrAugmented4: %.2f%% (%d)"
              ++ "\n\ttrRetrograded4: %.2f%% (%d)"
              ++ "\n\texact2: %.2f%% (%d)"
              ++ "\n\ttransposed2: %.2f%% (%d)"
              ++ "\n\ttonalTransped2: %.2f%% (%d)"
              ++ "\n\tinverted2: %.2f%% (%d)"
              ++ "\n\taugmented2: %.2f%% (%d)"
              ++ "\n\tretrograded2: %.2f%% (%d)"
              ++ "\n\trotated2: %.2f%% (%d)"
              ++ "\n\ttrInverted2: %.2f%% (%d)"
              ++ "\n\ttrAugmented2: %.2f%% (%d)"
              ++ "\n\ttrRetrograded2: %.2f%% (%d)"
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
           (percentage an exact4) (exact4 an)
           (percentage an transposed4) (transposed4 an)
           (percentage an tonalTransped4) (tonalTransped4 an)
           (percentage an inverted4) (inverted4 an)
           (percentage an augmented4) (augmented4 an)
           (percentage an retrograded4) (retrograded4 an)
           (percentage an rotated4) (rotated4 an)
           (percentage an trInverted4) (trInverted4 an)
           (percentage an trAugmented4) (trAugmented4 an)
           (percentage an trRetrograded4) (trRetrograded4 an)
           (percentage an exact2) (exact2 an)
           (percentage an transposed2) (transposed2 an)
           (percentage an tonalTransped2) (tonalTransped2 an)
           (percentage an inverted2) (inverted2 an)
           (percentage an augmented2) (augmented2 an)
           (percentage an retrograded2) (retrograded2 an)
           (percentage an rotated2) (rotated2 an)
           (percentage an trInverted2) (trInverted2 an)
           (percentage an trAugmented2) (trAugmented2 an)
           (percentage an trRetrograded2) (trRetrograded2 an)
           (otherPercentage an) (length $ unclassified an)

percentages :: AnalysisResult -> AnalysisPercentage
percentages an = AnP
          {
           nameP = name an
           , totalP = total an
           
           , exactP           =         percentage an exact
           , transposedP     =     percentage an transposed
           , tonalTranspedP  =  percentage an tonalTransped
           , invertedP       =       percentage an inverted
           , augmentedP      =      percentage an augmented
           , retrogradedP    =    percentage an retrograded
           , rotatedP        =        percentage an rotated
           , trInvertedP     =     percentage an trInverted
           , trAugmentedP    =    percentage an trAugmented
           , trRetrogradedP  =  percentage an trRetrograded
           -- 80P            =
           , exact8P         =         percentage an exact8
           , transposed8P    =    percentage an transposed8
           , tonalTransped8P = percentage an tonalTransped8
           , inverted8P      =      percentage an inverted8
           , augmented8P     =     percentage an augmented8
           , retrograded8P   =   percentage an retrograded8
           , rotated8P       =       percentage an rotated8
           , trInverted8P    =    percentage an trInverted8
           , trAugmented8P   =   percentage an trAugmented8
           , trRetrograded8P = percentage an trRetrograded8
           -- 60P            =
           , exact6P         =         percentage an exact6
           , transposed6P    =    percentage an transposed6
           , tonalTransped6P = percentage an tonalTransped6
           , inverted6P      =      percentage an inverted6
           , augmented6P     =     percentage an augmented6
           , retrograded6P   =   percentage an retrograded6
           , rotated6P       =       percentage an rotated6
           , trInverted6P    =    percentage an trInverted6
           , trAugmented6P   =   percentage an trAugmented6
           , trRetrograded6P = percentage an trRetrograded6
           -- 40P            =
           , exact4P         =         percentage an exact4
           , transposed4P    =    percentage an transposed4
           , tonalTransped4P = percentage an tonalTransped4
           , inverted4P      =      percentage an inverted4
           , augmented4P     =     percentage an augmented4
           , retrograded4P   =   percentage an retrograded4
           , rotated4P       =       percentage an rotated4
           , trInverted4P    =    percentage an trInverted4
           , trAugmented4P   =   percentage an trAugmented4
           , trRetrograded4P = percentage an trRetrograded4
           -- 20P            =
           , exact2P         =         percentage an exact2
           , transposed2P    =    percentage an transposed2
           , tonalTransped2P = percentage an tonalTransped2
           , inverted2P      =      percentage an inverted2
           , augmented2P     =     percentage an augmented2
           , retrograded2P   =   percentage an retrograded2
           , rotated2P       =       percentage an rotated2
           , trInverted2P    =    percentage an trInverted2
           , trAugmented2P   =   percentage an trAugmented2
           , trRetrograded2P = percentage an trRetrograded2
           , unclassifiedP   = otherPercentage an}

-- | Analyzing a pattern group with a pattern prototype (occ1.csv).
data AnalysisPercentage = AnP
  { nameP           :: !String -- ^ name associated with the analysis
  , totalP          :: !Int -- ^ all other occurences of the pattern
  -- 100P%
  , exactP          :: !Double -- ^ # of exact occurences
  , transposedP     :: !Double -- ^ # of *atonal* transpositions
  , tonalTranspedP  :: !Double -- ^ # of *tonal* transpositions
  , invertedP       :: !Double -- ^ # of inversions
  , augmentedP      :: !Double -- ^ # of augmentations
  , retrogradedP    :: !Double -- ^ # of retrogrades
  , rotatedP        :: !Double -- ^ # of rotations
  , trInvertedP     :: !Double -- ^ # of transposed inversions
  , trAugmentedP    :: !Double -- ^ # of transposed augmentations
  , trRetrogradedP  :: !Double -- ^ # of transposed retrogrades
  -- 80P%
  , exact8P         :: !Double -- ^ # of exact occurences
  , transposed8P    :: !Double -- ^ # of *atonal* transpositions
  , tonalTransped8P :: !Double -- ^ # of *tonal* transpositions
  , inverted8P      :: !Double -- ^ # of inversions
  , augmented8P     :: !Double -- ^ # of augmentations
  , retrograded8P   :: !Double -- ^ # of retrogrades
  , rotated8P       :: !Double -- ^ # of rotations
  , trInverted8P    :: !Double -- ^ # of transposed inversions
  , trAugmented8P   :: !Double -- ^ # of transposed augmentations
  , trRetrograded8P :: !Double -- ^ # of transposed retrogrades
  -- 60P%
  , exact6P         :: !Double -- ^ # of exact occurences
  , transposed6P    :: !Double -- ^ # of *atonal* transpositions
  , tonalTransped6P :: !Double -- ^ # of *tonal* transpositions
  , inverted6P      :: !Double -- ^ # of inversions
  , augmented6P     :: !Double -- ^ # of augmentations
  , retrograded6P   :: !Double -- ^ # of retrogrades
  , rotated6P       :: !Double -- ^ # of rotations
  , trInverted6P    :: !Double -- ^ # of transposed inversions
  , trAugmented6P   :: !Double -- ^ # of transposed augmentations
  , trRetrograded6P :: !Double -- ^ # of transposed retrogrades
  -- 40P%
  , exact4P         :: !Double -- ^ # of exact occurences
  , transposed4P    :: !Double -- ^ # of *atonal* transpositions
  , tonalTransped4P :: !Double -- ^ # of *tonal* transpositions
  , inverted4P      :: !Double -- ^ # of inversions
  , augmented4P     :: !Double -- ^ # of augmentations
  , retrograded4P   :: !Double -- ^ # of retrogrades
  , rotated4P       :: !Double -- ^ # of rotations
  , trInverted4P    :: !Double -- ^ # of transposed inversions
  , trAugmented4P   :: !Double -- ^ # of transposed augmentations
  , trRetrograded4P :: !Double -- ^ # of transposed retrogrades
  -- 20P%
  , exact2P         :: !Double -- ^ # of exact occurences
  , transposed2P    :: !Double -- ^ # of *atonal* transpositions
  , tonalTransped2P :: !Double -- ^ # of *tonal* transpositions
  , inverted2P      :: !Double -- ^ # of inversions
  , augmented2P     :: !Double -- ^ # of augmentations
  , retrograded2P   :: !Double -- ^ # of retrogrades
  , rotated2P       :: !Double -- ^ # of rotations
  , trInverted2P    :: !Double -- ^ # of transposed inversions
  , trAugmented2P   :: !Double -- ^ # of transposed augmentations
  , trRetrograded2P :: !Double -- ^ # of transposed retrogrades
  , unclassifiedP  :: !Double -- ^ filenames of all unclassified patterns
  }
  deriving (Generic)

instance ToNamedRecord AnalysisPercentage
instance DefaultOrdered AnalysisPercentage
