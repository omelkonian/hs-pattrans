{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Render (dumpAnalyses, renderOne, render) where

import Control.Monad (forM_, when)
import Numeric       (showFFloat)

-- Chart
import Data.Char (isDigit)
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart.Easy hiding (render)
import Graphics.Rendering.Chart.Backend.Cairo

-- CSV
import Data.Csv hiding ((.=))
import qualified Data.ByteString.Lazy as BL

-- MIDI
import MIDI (writeToMidi)

import Types
import Parser
import Analysis

-------------------
-- CSV files

-- | Whether to print absolute values or percentages in CSV fields.
data OutputMode = Absolute | Percentage

-- | Conversion from AnalysisResult to CSV.
instance ToNamedRecord (OutputMode, Analysis, AnalysisResult) where
  toNamedRecord (mode, curAnalysis, an) = namedRecord $
    [ ("name",  toField (name an))
    , ("total", toField tot) ] ++
    map (\(s,n) -> (toField s, showPercentage n))
        (orderedResults curAnalysis an) ++
    [ ("unclassified", showPercentage $ length (unclassified an)) ]
    where
      tot = total an

      showPercentage n = case mode of 
        Absolute   -> toField n
        Percentage -> toField $ showFFloat (Just 2) (fromIntegral n / fromIntegral tot * 100) ""

-- | Dump all files, relevant to a (group of) analyses.
dumpAnalyses :: String -> Bool -> Analysis -> [AnalysisResult] -> IO ()
dumpAnalyses fname expo curAnalysis as = do
  let resFields    = (toField . fst) <$> orderedResults curAnalysis (head as)
  let headerFields = header $ ["name", "total"] ++ resFields ++ ["unclassified"]
  -- File #1: absolute values
  BL.writeFile (fname ++ ".csv") $
    encodeByName headerFields (map (\a -> (Absolute, curAnalysis, a)) as)
  -- File #2: percentage values
  BL.writeFile (fname ++ "-percentages.csv") $
    encodeByName headerFields (map (\a -> (Percentage, curAnalysis, a)) as)
  -- File #3: unclassified indices
  let uncls = concatMap unclassified as
  writeFile "unclassified.txt" $ unlines (fst <$> uncls)
  when expo $ do
    emptyDirectory "unclassified"
    cd "unclassified" $
      forM_ uncls $ \(f, p) -> writeToMidi (f ++ ".mid") p

-- | Show instance for AnalysisResult.
instance {-# OVERLAPPING #-} Show (Analysis, AnalysisResult) where
  show (curAnalysis, an) =
       name an ++ " {"
    ++ "\n\ttotal: " ++ show tot
    ++ concat [ "\n\t" ++ s ++ ": " ++ showPercentage n
              | (s, n) <- orderedResults curAnalysis an ]
    ++ "\n\tother: " ++ showPercentage (length $ unclassified an)
    ++ "\n}"
    where
      tot = total an
      showPercentage n
        = showFFloat (Just 2) (fromIntegral n / fromIntegral tot * 100) ""
            ++ "% (" ++ show n ++ ")"

-------------------
-- Charts

-- | Visualize the results of analyzing a single pattern group in a pie chart.
renderOne :: Analysis -> PatternGroup -> AnalysisResult -> IO ()
renderOne curAnalysis (PatternGroup piece_n expert_n pattern_n _ _) an =
  cd (piece_n ++ "/" ++ expert_n) $
    render curAnalysis pattern_n an

-- | Visualize an arbitrary AnalysisResult.
render :: Analysis -> String -> AnalysisResult -> IO ()
render curAnalysis fname an
  | total an == 0
  = return ()
  | otherwise
  = do toFile def (fname ++ ".png") $ do
         pie_plot . pie_data                           .= values
         pie_plot . pie_colors                         .= map opaque colours
         pie_plot . pie_label_line_style . line_width  .= 0.5
         pie_plot . pie_label_style      . font_size   .= 14
  where
    colours :: [Colour Double]
    colours =
      -- transformations (100%, 80%, 60%, 40%)
      take 50 (cycle [pink, darkblue, darkred, green, darkorange, darkcyan, darkmagenta, brown, darkviolet, darkorange])
      -- other
      ++ [black]

    values :: [PieItem]
    values =
      [ pitem_value  .~ v
      $ pitem_label  .~ (if v > 2 then formatLabel s else "")
      $ pitem_offset .~ 30
      $ def
      | (s, v) <- map (fmap (\n -> (fromIntegral n / fromIntegral (total an) * 100)))
                      (orderedResults curAnalysis an ++ [("other", length $ unclassified an)])
      ]

    formatLabel :: String -> String
    formatLabel s = trS ++ case n of
                             ('0':_) -> " ~ " ++ show ((read n :: Int) * 10) ++ "%"
                             _       -> ""
      where (trS, n) = break isDigit s
