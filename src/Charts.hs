module Charts (renderOne, renderAll) where

import Control.Monad (forM_, when)

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
    render pattern_n (piece_n ++ ":" ++ expert_n ++ ":" ++ pattern_n) an

-- | Visualize the result of analyzing multiple pattern groups in a single pie chart.
renderAll :: Bool -> String -> AnalysisResult -> IO ()
renderAll expo s an = do
  -- Render overview chart
  render "ALL" ("ALL: " ++ s) an
  -- Store unclassified patterns
  when expo $ do
    emptyDirectory "unclassified"
    let uncls = unclassified an
    writeFile "unclassified.txt" $ unlines (fst <$> uncls)
    cd "unclassified" $
      forM_ uncls $ \(f, p) -> writeToMidi (f ++ ".mid") p

render :: String -> String -> AnalysisResult -> IO ()
render fname title an
  | total an == 0
  = return ()
  | otherwise
  = do toFile def (fname ++ ".png") $ do
         pie_title .= title
         pie_plot . pie_data .= values
         pie_plot . pie_colors .= map opaque colours
  where
    colours :: [Colour Double]
    colours =
      -- transformations (100%, 80%, 60%, 40%)
      take 50 (cycle [pink, darkblue, darkred, green, darkorange, darkcyan, darkmagenta, brown, darkviolet, darkorange])
      -- other
      ++ [black]

    values :: [PieItem]
    values =
      [ pitem_value .~ v
      $ pitem_label .~ (if v > 0 then s else "")
      $ pitem_offset .~ 40
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


