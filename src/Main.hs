import Control.Monad (forM, forM_)

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
  forM_ allPts $ \(_, pts) ->
    forM_ pts $ \pt -> do
      let an = analysePatternType pt
      let title = piece_name pt ++ ":" ++ expert_name pt ++ ":" ++ pattern_name pt
      -- display on terminal
      putStrLn $ title ++ " " ++ show an
      -- produce pie chart
      render pt title an

