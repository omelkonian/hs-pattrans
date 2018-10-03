import Control.Monad (forM)

import Types
import Parser
import Analysis
import Charts

main :: IO ()
main = do
  runAnalysis "docs/out/experts" (parseMirex Monophonic)
  -- runAnalysis "docs/out/algorithms" parseAlgo

runAnalysis :: FilePath -> IO [PatternGroup] -> IO ()
runAnalysis f_root parser = do
    -- Parse dataset to retrieve all pattern groups.
    allPatternGroups <- parser
    -- Analyse individual pattern groups.
    cd f_root $ do
      analyses <-
        forM allPatternGroups $ \pg -> do
          let an = analysePatternGroup pg
          putStrLn $ getTitle pg ++ " " ++ show an -- display on terminal
          renderOne pg an -- produce pie chart
          return an

      -- Combine all individual analyses and render in one chart.
      let finalAn = combineAnalyses analyses
      putStrLn $ "ALL " ++ show finalAn
      renderAll f_root finalAn
