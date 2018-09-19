module Analysis where

import Data.List (intersperse)
import Text.Printf (printf)

import Types

-- | Analyzing a pattern group with a pattern prototype (occ1.csv).
data AnalysisResult = AnalysisResult
  { total :: Int -- ^ all other occurences of the pattern
  , exact :: Int -- ^ exact occurences (i.e. transposed in time)
  }

-- | Analyze a single pattern group
analysePatternGroup :: PatternGroup -> AnalysisResult
analysePatternGroup (PatternGroup _ _ _ base pats) =
  AnalysisResult (length pats) (length $ filter (== base) pats)

-- | Combine analyses from different pattern groups.
combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl (<+>) (AnalysisResult 0 0)
  where
    AnalysisResult tot ex <+> AnalysisResult tot' ex'
      = AnalysisResult (tot + tot') (ex + ex')

-- | Get the percentage of exact pattern occurences from an analysis result.
exactPercentage :: AnalysisResult -> Double
exactPercentage (AnalysisResult tot ex) =
  (fromIntegral ex / fromIntegral tot * 100)

-- | Pretty-print analysis.
instance Show AnalysisResult where
  show an =
    printf "{ exact: %.2f%% (%d out of %d) }" (exactPercentage an) (exact an) (total an)

-- | Get a title, unique to the given PatternGroup, in the following format:
-- <piece>:<expert>:<pattern>.
getTitle :: PatternGroup -> String
getTitle (PatternGroup piece_n expert_n pattern_n _ _) =
  concat $ intersperse ":" [piece_n, expert_n, pattern_n]
