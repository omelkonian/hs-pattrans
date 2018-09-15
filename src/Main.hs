import Control.Monad (forM_)
import System.Directory

import Parser
import Analysis

main :: IO ()
main = do
  setCurrentDirectory "data/pieces"
  fnames <- listDirs
  forM_ fnames $ \fname -> do
    putStrLn $ "\n********** " ++ fname ++ " **********\n"
    -- Parse
    (music, patternTypes) <- parseMirex fname Monophonic
    -- Analyse
    forM_ patternTypes $ \pt ->
      putStrLn $ name pt ++ " " ++ show (analysePatternType pt)

