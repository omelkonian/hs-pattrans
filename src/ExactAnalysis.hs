{-# LANGUAGE DeriveGeneric, FlexibleInstances, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ExactAnalysis where

import Data.List (foldl')
import Text.Printf (printf)

import GHC.Generics (Generic)
import Data.Csv

import Types
import Transformations

data AnalysisResult = An
  { name           :: !String -- ^ name associated with the analysis
  , total          :: !Int -- ^ all other occurences of the pattern
  , exact          :: !Int -- ^ # of exact occurences
  , exact8         :: !Int -- ^ # of exact occurences
  , exact6         :: !Int -- ^ # of exact occurences
  , exact4         :: !Int -- ^ # of exact occurences
  , exact2         :: !Int -- ^ # of exact occurences
  , exact1         :: !Int -- ^ # of exact occurences
  , exact05         :: !Int -- ^ # of exact occurences
  , exact0         :: !Int -- ^ # of exact occurences
  , unclassified  :: ![(String, Pattern)] -- ^ filenames of all unclassified patterns
  }
  deriving (Generic)

instance ToNamedRecord AnalysisResult
instance DefaultOrdered AnalysisResult

instance ToField [(String, Pattern)] where
  toField = toField . length

defAn :: AnalysisResult
defAn = An "" 0 0 0 0 0 0 0 0 0 []

singleAn :: AnalysisResult
singleAn = defAn {total = 1}

combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl' (<+>) defAn
  where
    An _ tot
       m1 m2 m3 m4 m5 m6 m7 m8 xs
      <+> An _ tot' n1 n2 n3 n4 n5 n6 n7 n8 ys
      = An "" (tot+tot') (m1+n1) (m2+n2) (m3+n3) (m4+n4) (m5+n5) (m6+n6) (m7+n7) (m8+n8) (xs++ys)

analysePatternGroup :: PatternGroup -> IO AnalysisResult
analysePatternGroup pg@(PatternGroup _ _ _ base pats) =
  returnAnalyses $ map check (zip [2..] pats)
  where
    returnAnalyses res = return (combineAnalyses res) { name = show pg }

    -- Check which equivalence class a pattern belongs to.
    check (i, p)

      | (base <=> p) (exactOf ~~ 1)            = singleAn { exact = 1 }
      | (base <=> p) (exactOf ~~ 0.8)          = singleAn { exact8 = 1 }
      | (base <=> p) (exactOf ~~ 0.6)          = singleAn { exact6 = 1 }
      | (base <=> p) (exactOf ~~ 0.4)          = singleAn { exact4 = 1 }
      | (base <=> p) (exactOf ~~ 0.2)          = singleAn { exact2 = 1 }
      | (base <=> p) (exactOf ~~ 0.1)          = singleAn { exact2 = 1 }
      | (base <=> p) (exactOf ~~ 0.05)          = singleAn { exact2 = 1 }
      | (base <=> p) (exactOf ~~ 0.0)          = singleAn { exact2 = 1 }

      | otherwise
      = defAn {total = 1, unclassified = [(show pg ++ ":" ++ show i, p)]}
instance Show AnalysisResult where
  show an =
    printf ("%s { \n\ttotal: %d"
              ++ "\n\texact: %.2f%% (%d)"
              ++ "\n\texact8: %.2f%% (%d)"
              ++ "\n\texact6: %.2f%% (%d)"
              ++ "\n\texact4: %.2f%% (%d)"
              ++ "\n\texact2: %.2f%% (%d)"
              ++ "\n\texact1: %.2f%% (%d)"
              ++ "\n\texact05: %.2f%% (%d)"
              ++ "\n\texact0: %.2f%% (%d)"
              ++ "\n\tother: %.2f%% (%d)"
              ++ "\n}")
           (name an) (total an)
           (percentage an exact) (exact an)
           (percentage an exact8) (exact8 an)
           (percentage an exact6) (exact6 an)
           (percentage an exact4) (exact4 an)
           (percentage an exact2) (exact2 an)
           (percentage an exact) (exact an)

           (percentage an exact05) (exact05 an)

           (percentage an exact0) (exact0 an)

           (otherPercentage an) (length $ unclassified an)


-- | Get the percentage of an equivalence class from an analysis result.
percentage :: AnalysisResult -> (AnalysisResult -> Int) -> Double
percentage an f = fromIntegral (f an) / fromIntegral (total an) * 100

-- | Get the percentage of unclassified patterns from an analysis result.
otherPercentage :: AnalysisResult -> Double
otherPercentage an =
  fromIntegral (length $ unclassified an) / fromIntegral (total an) * 100
