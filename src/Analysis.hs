module Analysis ( analysePatternType
                , AnalysisResult(..)
                ) where

import Text.Printf
import Parser (PatternType(..))

data AnalysisResult = AnalysisResult { total :: Int
                                     , exact :: Int
                                     }
instance Show AnalysisResult where
  show (AnalysisResult tot ex) =
    printf "{ exact: %.2f%% }" (fromIntegral ex / fromIntegral tot * 100 :: Double)

analysePatternType :: PatternType -> AnalysisResult
analysePatternType (PatternType _ base pats) =
  AnalysisResult (length pats) (length $ filter (== base) pats)

