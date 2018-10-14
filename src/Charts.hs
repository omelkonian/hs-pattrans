module Charts (renderOne, renderAll) where

import Control.Monad (forM_, when)

import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart.Easy hiding (render)
import Graphics.Rendering.Chart.Backend.Cairo

import Types
import Parser
import Analysis
import Export (writeToMidi)

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
    colours = [ -- exacts
               pink
               -- transformations
             , blue, red, green, yellow, cyan, magenta, brown, gray, goldenrod
               -- approximate equality
             , darkblue, darkred, darkgreen, yellowgreen, darkcyan, darkmagenta
             , saddlebrown, darkgray, darkgoldenrod
               -- other
             , black
             ]

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
                                         , ("aproxEq9", approxEq9)
                                         , ("aproxEq8", approxEq8)
                                         , ("aproxEq7", approxEq7)
                                         , ("aproxEq6", approxEq6)
                                         , ("aproxEq5", approxEq5)
                                         , ("aproxEq4", approxEq4)
                                         , ("aproxEq3", approxEq3)
                                         , ("aproxEq2", approxEq2)
                                         , ("aproxEq1", approxEq1)
                                         ]
      , (s, v) <- xs ++ [("other", otherPercentage an)]
      ]


