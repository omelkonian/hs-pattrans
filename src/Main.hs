import Control.Monad (forM)

import Parser
import Analysis
import Charts

main :: IO ()
main = do
  -- Parse the MIREX dataset, retrieving all pattern groups.
  allPatternGroups <- cd "data/pieces" $ do
    fnames <- listDirs
    concat <$> forM fnames ((snd <$>) . parseMirex Monophonic)

  -- Analyse individual pattern groups.
  analyses <-
    forM allPatternGroups $ \pg -> do
      let an = analysePatternGroup pg
      putStrLn $ getTitle pg ++ " " ++ show an -- display on terminal
      renderOne pg an -- produce pie chart
      return an

  -- Combine all individual analyses and render in one chart.
  let finalAn = combineAnalyses analyses
  putStrLn $ "ALL " ++ show finalAn
  renderAll finalAn
