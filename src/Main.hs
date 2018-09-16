import Control.Monad (forM)

import Parser
import Analysis
import Charts

main :: IO ()
main = do
  -- Parse
  allPts <- cd "data/pieces" $ do
    fnames <- listDirs
    forM fnames $ \fname -> do
      parseMirex fname Monophonic

  -- Analyse
  analyses <-
    forM allPts $ \(_, pts) -> do
      forM pts $ \pt -> do
        let an = analysePatternType pt
        let title = piece_name pt ++ ":" ++ expert_name pt ++ ":" ++ pattern_name pt
        putStrLn $ title ++ " " ++ show an -- display on terminal
        renderOne pt an -- produce pie chart
        return an

  let finalAn = combineAnalyses (concat analyses)
  putStrLn $ "ALL " ++ show finalAn
  renderAll finalAn

