module Charts (renderOne, renderAll) where

import System.Directory

import Graphics.Rendering.Chart.Easy hiding (render)
import Graphics.Rendering.Chart.Backend.Cairo

import Parser
import Analysis

renderOne :: PatternType -> AnalysisResult -> IO ()
renderOne pt =
  render ("output/" ++ piece_n ++ "/" ++ expert_n) pattern_n title
  where
    PatternType piece_n expert_n pattern_n _ _ = pt
    title = piece_n ++ ":" ++ expert_n ++ ":" ++ pattern_n

renderAll :: AnalysisResult -> IO ()
renderAll = render "output" "ALL" "ALL"

render :: FilePath -> String -> String -> AnalysisResult -> IO ()
render root fname title an = do
  createDirectoryIfMissing True root
  toFile def (root ++ "/" ++ fname ++ ".png") $ do
    pie_title .= title
    pie_plot . pie_data .= values
  where
    values :: [PieItem]
    values =
      [ pitem_value .~ v
      $ pitem_label .~ s
      $ pitem_offset .~ (if o then 25 else 0)
      $ def
      | (s, v, o) <- [("exact", exPer, True), ("other", 100.0 - exPer, False)]
      ]
      where exPer = exactPercentage an
