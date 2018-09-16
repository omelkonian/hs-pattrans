module Analysis ( analysePatternType
                , AnalysisResult(..), combineAnalyses
                , exactPercentage
                ) where

import Text.Printf
import Parser (PatternType(..))

data AnalysisResult = AnalysisResult { total :: Int
                                     , exact :: Int
                                     }

combineAnalyses :: [AnalysisResult] -> AnalysisResult
combineAnalyses = foldl1 (<+>)
  where
    AnalysisResult tot ex <+> AnalysisResult tot' ex'
      = AnalysisResult (tot + tot') (ex + ex')


exactPercentage :: AnalysisResult -> Double
exactPercentage (AnalysisResult tot ex) =
  (fromIntegral ex / fromIntegral tot * 100)

analysePatternType :: PatternType -> AnalysisResult
analysePatternType (PatternType _ _ _ base pats) =
  AnalysisResult (length pats) (length $ filter (== base) pats)

instance Show AnalysisResult where
  show an =
    printf "{ exact: %.2f%% (%d out of %d) }" (exactPercentage an) (exact an) (total an)
