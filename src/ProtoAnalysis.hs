{-# LANGUAGE DeriveGeneric, FlexibleInstances, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module ProtoAnalysis where

import GHC.Generics (Generic)

import Data.List (foldl')
import Text.Printf (printf)
import Data.Csv
import Types
import Transformations
data AnalysisResultSelf = An
  { name           :: !String -- ^ name associated with the analysis
  , total          :: !Int -- ^ all other occurences of the pattern
  , selfinverted   :: !Int
  , selfretrograded :: !Int
  , selfrotated :: !Int

  , selfinverted8   :: !Int
  , selfretrograded8 :: !Int
  , selfrotated8 :: !Int

  , selfinverted6   :: !Int
  , selfretrograded6 :: !Int
  , selfrotated6 :: !Int
  , unclassified  :: ![(String, Pattern)] -- ^ filenames of all unclassified patterns
  }
  deriving (Generic)

instance ToNamedRecord AnalysisResultSelf
instance DefaultOrdered AnalysisResultSelf

instance ToField [(String, Pattern)] where
  toField = toField . length
defAn :: AnalysisResultSelf
defAn = An "" 0 0 0 0 0 0 0 0 0 0 []

singleAn :: AnalysisResultSelf
singleAn = defAn {total = 1}

combineAnalyses :: [AnalysisResultSelf] -> AnalysisResultSelf
combineAnalyses = foldl' (<+>) defAn
  where
    An _ tot
       m1 m2 m3 m4 m5 m6 m7 m8 m9 xs
      <+> An _ tot' n1 n2 n3 n4 n5 n6 n7 n8 n9 ys
      = An "" (tot+tot') (m1+n1) (m2+n2) (m3+n3) (m4+n4) (m5+n5) (m6+n6) (m7+n7) (m8+n8) (m9+n9) (xs++ys)

analysePatternGroup :: PatternGroup -> IO AnalysisResultSelf
analysePatternGroup pg@(PatternGroup _ _ _ base pats) =
  returnAnalyses $ map check (zip [2..] pats)
  where
    returnAnalyses res = return (combineAnalyses res) { name = show pg }

    -- Check which equivalence class a pattern belongs to.
    check (i, p)

      | (base <=> base) (inversionOf ~~ 1)        = singleAn { selfinverted = 1 }
      | (base <=> base) (retrogradeOf ~~ 1)       = singleAn { selfretrograded = 1 }
      | (base <=> base) (rotationOf ~~ 1)       = singleAn { selfrotated = 1 }

      | (base <=> base) (inversionOf ~~ 0.8)        = singleAn { selfinverted8 = 1 }
      | (base <=> base) (retrogradeOf ~~ 0.8)       = singleAn { selfretrograded8 = 1 }
      | (base <=> base) (rotationOf ~~ 0.8)       = singleAn { selfrotated8 = 1 }

      | (base <=> base) (inversionOf ~~ 0.6)        = singleAn { selfinverted6 = 1 }
      | (base <=> base) (retrogradeOf ~~ 0.6)       = singleAn { selfretrograded6 = 1 }
      | (base <=> base) (rotationOf ~~ 0.6)       = singleAn { selfrotated6 = 1 }
      | otherwise
      = defAn {total = 1, unclassified = [(show pg ++ ":" ++ show i, p)]}
instance Show AnalysisResultSelf where
  show an =
    printf ("%s { \n\ttotal: %d"
              ++ "\n\tselfinvert: %.2f%% (%d)"
              ++ "\n\tselfrotated: %.2f%% (%d)"
              ++ "\n\teselfretro: %.2f%% (%d)"

              ++ "\n\tselfinvert8: %.2f%% (%d)"
              ++ "\n\tselfrotated8: %.2f%% (%d)"
              ++ "\n\teselfretro8: %.2f%% (%d)"

              ++ "\n\tselfinvert6: %.2f%% (%d)"
              ++ "\n\tselfrotated6: %.2f%% (%d)"
              ++ "\n\teselfretro6: %.2f%% (%d)"
              ++ "\n\tother: %.2f%% (%d)"
              ++ "\n}")
           (name an) (total an)
           (percentage an selfinverted) (selfinverted an)
           (percentage an selfretrograded) (selfretrograded an)
           (percentage an selfrotated) (selfrotated an)

           (percentage an selfinverted8) (selfinverted8 an)
           (percentage an selfretrograded8) (selfretrograded8 an)
           (percentage an selfrotated8) (selfrotated8 an)

           (percentage an selfinverted6) (selfinverted6 an)
           (percentage an selfretrograded6) (selfretrograded6 an)
           (percentage an selfrotated6) (selfrotated6 an)
           (otherPercentage an) (length $ unclassified an)


-- | Get the percentage of an equivalence class from an analysis result.
percentage :: AnalysisResultSelf -> (AnalysisResultSelf -> Int) -> Double
percentage an f = fromIntegral (f an) / fromIntegral (total an) * 100

-- | Get the percentage of unclassified patterns from an analysis result.
otherPercentage :: AnalysisResultSelf -> Double
otherPercentage an =
  fromIntegral (length $ unclassified an) / fromIntegral (total an) * 100
