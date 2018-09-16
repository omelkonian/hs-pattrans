module Analysis ( analysePatternType
                , AnalysisResult(..)
                , exactPercentage
                ) where

import Text.Printf
import Parser (PatternType(..))

data AnalysisResult = AnalysisResult { total :: Int
                                     , exact :: Int
                                     }

exactPercentage :: AnalysisResult -> Double
exactPercentage (AnalysisResult tot ex) =
  (fromIntegral ex / fromIntegral tot * 100)

instance Show AnalysisResult where
  show = printf "{ exact: %.2f%% }" . exactPercentage

analysePatternType :: PatternType -> AnalysisResult
analysePatternType (PatternType _ _ _ base pats) =
  AnalysisResult (length pats) (length $ filter (== base) pats)

