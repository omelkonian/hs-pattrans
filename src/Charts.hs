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
renderAll :: AnalysisResult -> IO ()
renderAll = render "ALL" "ALL"

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
                                         , ("inverted", inverted)
                                         , ("augmented", augmented)
                                         , ("retrograded", retrograded)
                                         , ("rotated", rotated)
                                         , ("trInverted", trInverted)
                                         , ("trAugmented", trAugmented)
                                         , ("trRetrograded", trRetrograded) ]
      , (s, v) <- xs ++ [("other", 100.0 - sum (snd <$> xs))]
      ]


