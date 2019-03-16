module Charts (renderOne, renderAll) where

import Control.Monad (forM_, when)

import Data.Char (isDigit)
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart.Easy hiding (render)
import Graphics.Rendering.Chart.Backend.Cairo

import Types
import Parser
import Analysis
import MIDI (writeToMidi)

-- | Visualize the results of analyzing a single pattern group in a pie chart.
renderOne :: PatternGroup -> AnalysisResult -> IO ()
renderOne (PatternGroup piece_n expert_n pattern_n _ _) an =
  cd (piece_n ++ "/" ++ expert_n) $
    render pattern_n an

-- | Visualize the result of analyzing multiple pattern groups in a single pie chart.
renderAll :: Bool -> AnalysisResult -> IO ()
renderAll expo an = do
  -- Render overview chart
  render "ALL" an
  -- Store unclassified patterns
  when expo $ do
    emptyDirectory "unclassified"
    let uncls = unclassified an
    writeFile "unclassified.txt" $ unlines (fst <$> uncls)
    cd "unclassified" $
      forM_ uncls $ \(f, p) -> writeToMidi (f ++ ".mid") p

render :: String -> AnalysisResult -> IO ()
render fname an
  | total an == 0
  = return ()
  | otherwise
  = do toFile def (fname ++ ".png") $ do
         pie_plot . pie_data   .= values
         pie_plot . pie_colors .= map opaque colours
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
      | let xs = (percentage an <$>) <$> [ ("exact", exact)
                                         , ("transposed", transposed)
                                         , ("tonalTransped", tonalTransped)
                                         , ("inverted", inverted)
                                         , ("augmented", augmented)
                                         , ("retrograded", retrograded)
                                         , ("rotated", rotated)
                                         , ("trInverted", trInverted)
                                         , ("trAugmented", trAugmented)
                                         , ("trRetrograded", trRetrograded)
                                         , ("exact8", exact8)
                                         , ("transposed8", transposed8)
                                         , ("tonalTransped8", tonalTransped8)
                                         , ("inverted8", inverted8)
                                         , ("augmented8", augmented8)
                                         , ("retrograded8", retrograded8)
                                         , ("rotated8", rotated8)
                                         , ("trInverted8", trInverted8)
                                         , ("trAugmented8", trAugmented8)
                                         , ("trRetrograded8", trRetrograded8)
                                         , ("exact6", exact6)
                                         , ("transposed6", transposed6)
                                         , ("tonalTransped6", tonalTransped6)
                                         , ("inverted6", inverted6)
                                         , ("augmented6", augmented6)
                                         , ("retrograded6", retrograded6)
                                         , ("rotated6", rotated6)
                                         , ("trInverted6", trInverted6)
                                         , ("trAugmented6", trAugmented6)
                                         , ("trRetrograded6", trRetrograded6)
                                         , ("exact4", exact4)
                                         , ("transposed4", transposed4)
                                         , ("tonalTransped4", tonalTransped4)
                                         , ("inverted4", inverted4)
                                         , ("augmented4", augmented4)
                                         , ("retrograded4", retrograded4)
                                         , ("rotated4", rotated4)
                                         , ("trInverted4", trInverted4)
                                         , ("trAugmented4", trAugmented4)
                                         , ("trRetrograded4", trRetrograded4)
                                         , ("exact2", exact2)
                                         , ("transposed2", transposed2)
                                         , ("tonalTransped2", tonalTransped2)
                                         , ("inverted2", inverted2)
                                         , ("augmented2", augmented2)
                                         , ("retrograded2", retrograded2)
                                         , ("rotated2", rotated2)
                                         , ("trInverted2", trInverted2)
                                         , ("trAugmented2", trAugmented2)
                                         , ("trRetrograded2", trRetrograded2)
                                         ]
      , (s, v) <- xs ++ [("other", otherPercentage an)]
      ]

    formatLabel :: String -> String
    formatLabel s = trS ++ case n of
                             [] -> ""
                             _  -> " ~ " ++ show ((read n :: Int) * 10) ++ "%"
      where (trS, n) = break isDigit s
