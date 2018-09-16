module Charts where

import Control.Monad (void)
import System.Directory

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Parser
import Analysis

render :: PatternType -> String -> AnalysisResult -> IO ()
render pt title an = do
  let PatternType piece_n expert_n pattern_n _ _ = pt
  let dir = "output/" ++ piece_n ++ "/" ++ expert_n ++ "/"
  createDirectoryIfMissing True dir
  toFile def (dir ++ pattern_n ++ ".png") $ do
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
