import Data.Semigroup ((<>))
import Control.Monad (forM, when)
import Options.Applicative

import Types
import Parser
import Analysis
import Charts

-- | Command-line options.
data Options = Options { experts    :: Bool -- ^ analyze expert dataset
                       , algorithms :: Bool -- ^ analyze algorithm dataset
                       , export     :: Bool -- ^ export MIDI files
                       }

parseOpts :: Parser Options
parseOpts = Options
  <$> switch
      (  long "experts"
      <> short 'E'
      <> help "Analyze the expert dataset"
      )
  <*> switch
      (  long "algorithms"
      <> short 'A'
      <> help "Analyze the algorithm dataset"
      )
  <*> switch
      (  long "export"
      <> short 'X'
      <> help "Export MIDI files"
      )

main :: IO ()
main = do
  Options {experts = expe, algorithms = alg, export = expo} <- execParser opts
  when expe $
    runAnalysis expo "docs/out/experts" (parseMirex Monophonic)
  when alg $
    runAnalysis expo "docs/out/algorithms" parseAlgo

  where
    opts :: ParserInfo Options
    opts = info (parseOpts <**> helper)
                (  fullDesc
                <> progDesc "Run analysis on the MIREX dataset"
                <> header "hs-mirex: a tool for music pattern discovery"
                )

runAnalysis :: Bool -> FilePath -> IO [PatternGroup] -> IO ()
runAnalysis expo f_root parser = do
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
      renderAll expo f_root finalAn
