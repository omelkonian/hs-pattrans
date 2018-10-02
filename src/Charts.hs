module Charts (renderOne, renderAll) where

import Graphics.Rendering.Chart.Easy hiding (render)
import Graphics.Rendering.Chart.Backend.Cairo

import Types
import Parser
import Analysis

-- | Visualize the results of analyzing a single pattern group in a pie chart.
renderOne :: PatternGroup -> AnalysisResult -> IO ()
renderOne (PatternGroup piece_n expert_n pattern_n _ _) an =
  cd (piece_n ++ "/" ++ expert_n) $
    render pattern_n (piece_n ++ ":" ++ expert_n ++ ":" ++ pattern_n) an

-- | Visualize the result of analyzing multiple pattern groups in a single pie chart.
renderAll :: String -> AnalysisResult -> IO ()
renderAll s an = do
  -- Render overview chart
  render "ALL" ("ALL: " ++ s) an
  -- Store unclassified patterns
  writeFile "unclassified.txt" $ unlines (unclassified an)

render :: String -> String -> AnalysisResult -> IO ()
render fname title an
  | total an == 0
  = return ()
  | otherwise
  = do toFile def (fname ++ ".png") $ do
         pie_title .= title
         pie_plot . pie_data .= values
  where
    values :: [PieItem]
    values =
      [ pitem_value .~ v
      $ pitem_label .~ s
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
                                         , ("aproxEq2", approxEq2)
                                         , ("aproxEq4", approxEq4)
                                         , ("aproxEq6", approxEq6)
                                         , ("aproxEq8", approxEq8)
                                         , ("aproxEq10", approxEq10)
                                         , ("aproxEq12", approxEq12)
                                         , ("aproxEq14", approxEq14)
                                         , ("aproxEq16", approxEq16)
                                         ]
      , (s, v) <- xs ++ [("other", 100.0 - sum (snd <$> xs))]
      ]


