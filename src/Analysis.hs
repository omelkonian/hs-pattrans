{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Analysis where

import GHC.Generics  (Generic)

import Data.Char     (isDigit)
import Data.Foldable (msum)
import Data.List     (sortBy, elemIndex)
import qualified Data.Map as M

import Types
import Transformations

type ApproxLevel = Float
type Analysis    = ([(String, Float -> Check Pattern)], [ApproxLevel])

data AnalysisResult = AnalysisResult
  { name         :: String
  , results      :: M.Map String Int
  , unclassified :: [(String, Pattern)]
  }
  deriving Generic

total :: AnalysisResult -> Int
total an = sum $ length (unclassified an) : M.elems (results an)

instance {-# OVERLAPPING #-} Show (String, ApproxLevel) where
  show (fn, l) = fn ++ showApprox l
    where
      showApprox :: Float -> String
      showApprox = shorten . show
        where shorten ('0':'.':d:_)   = ['0',d]
              shorten ('1':'.':'0':_) = "1"
              shorten _               = error "showApprox: invalid approximation level"

-- | Combining analysis results.
(<+>) :: M.Map String Int -> M.Map String Int -> M.Map String Int
(<+>) = M.unionWith (+)

instance Monoid AnalysisResult where
  mempty = AnalysisResult
    { name         = ""
    , results      = M.empty
    , unclassified = []
    }
  mappend a a' = AnalysisResult
    { name         = name a ++ name a'
    , results      = results a <+> results a'
    , unclassified = unclassified a ++ unclassified a'
    }

-- | An empty result includes map entries for all analyses.
emptyRes :: Analysis -> M.Map String Int
emptyRes (as, ls) = M.fromList $ map (\s -> (s, 0)) [show (fn, l) | l <- ls, (fn, _) <- as]

-- | Analyse a single pattern.
analysePattern :: Analysis -> (Pattern, Pattern, String) -> AnalysisResult
analysePattern an@(analyses, approxLvls) (base, p, notFound) =
  case msum [ go (show (fn, lvl)) (f lvl)
            | lvl     <- approxLvls
            , (fn, f) <- analyses ] of
    Nothing  -> AnalysisResult "" (emptyRes an) [(notFound, p)]
    Just res -> res
  where
    go :: String -> Check Pattern -> Maybe AnalysisResult
    go s ch | (base <=> p) ch
            = Just $ AnalysisResult "" (emptyRes an <+> M.singleton s 1) []
            | otherwise
            = Nothing

-- | Analyse a pattern group.
analysePatternGroup :: Analysis -> PatternGroup -> AnalysisResult
analysePatternGroup analysis pg@(PatternGroup _ _ _ base pats)
  = (mconcat (map check (zip [2..] pats))) { name = show pg }
  where
    check (i, p) = analysePattern analysis (base, p, show pg ++ ":" ++ show i)

-- | Get the results of an analysis, ordered by their occurence in its definition.
orderedResults :: Analysis -> AnalysisResult -> [(String, Int)]
orderedResults curAnalysis =
  sortBy (\(s1,_) (s2,_) -> cmpAnalyses s1 s2) . M.toList . results
  where
    cmpAnalyses :: String -> String -> Ordering
    cmpAnalyses s1 s2 =
      case compare i i' of
        EQ -> case compare (getIndex s) (getIndex s') of
                EQ -> error "cmp: duplicate analyses" 
                o  -> o
        LT -> GT
        GT -> LT
      where
        [(s, i),(s', i')] = break isDigit <$> [s1, s2]
        ordAn = map fst $ fst curAnalysis

        getIndex :: String -> Int
        getIndex x = case x `elemIndex` ordAn of 
                       Just j  -> j
                       Nothing -> length ordAn

--------------------
-- Example analyses

fullAnalysis :: Analysis
fullAnalysis =
  ( [ ("exact",         (exactOf ~~))
    , ("transposed",    (transpositionOf ~~))
    , ("tonalTransped", (tonalTranspOf ~~))
    , ("inverted",      (inversionOf ~~))
    , ("augmented",     (augmentationOf ~~))
    , ("retrograded",   (retrogradeOf ~~))
    , ("rotated",       (rotationOf ~~))
    , ("trInverted",    (trInversionOf ~~))
    , ("trAugmented",   (trAugmentationOf ~~))
    , ("trRetrograded", (trRetrogradeOf ~~))
    ]
  , [1,0.8..0.2]
  )

exactAnalysis :: Analysis 
exactAnalysis =
  ( [("exact", (exactOf ~~))]
  , [1,0.8..0.2] ++ [0.1,0.05]
  )

protoAnalysis :: Analysis
protoAnalysis =
  ( [ ("inverted",      (inversionOf ~~))
    , ("retrograded",   (retrogradeOf ~~))
    , ("rotated",       (rotationOf ~~))
    ]
  , [1,0.8,0.6]
  )

compoAnalysis :: Analysis
compoAnalysis =
  ( [ ("exact",          (exactOf ~~))
    , ("transposed",     (transpositionOf ~~))
    , ("tonalTransped",  (tonalTranspOf ~~))
    , ("inverted",       (inversionOf ~~))
    , ("augmented",      (augmentationOf ~~))
    , ("retrograded",    (retrogradeOf ~~))
    , ("rotated",        (rotationOf ~~))
    , ("trInverted",     (trInversionOf ~~))
    , ("trAugmented",    (trAugmentationOf ~~))
    , ("trRetrograded",  (trRetrogradeOf ~~))
    , ("trtonAugmented", (trtonAugmentationOf ~~))
    , ("trtonRotated",   (trtonRotationOf ~~))
    ]
  , [1]
  )

approx6Analysis :: Analysis
approx6Analysis = fmap (filter (>= 0.6)) fullAnalysis

