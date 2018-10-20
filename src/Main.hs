import Data.Semigroup ((<>))
import Control.Monad (when, forM)
import Options.Applicative
import Data.Csv (encodeDefaultOrderedByName)
import qualified Data.ByteString.Lazy as BL

import Types
import Parser
import Analysis
import Charts

-- | Command-line options.
data Options = Options { experts    :: Bool -- ^ analyze expert dataset
                       , algorithms :: Bool -- ^ analyze algorithm dataset
                       , classical  :: Bool -- ^ analyze classical dataset
                       , folk       :: Bool -- ^ analyze dutch folk dataset
                       , random     :: Bool -- ^ analyze random datasets
                       , export     :: Bool -- ^ export MIDI files
                       , progress   :: Bool -- ^ whether to show progress bar
                       }

parseOpts :: Parser Options
parseOpts = Options
  <$> switch (  long "experts"
             <> short 'E'
             <> help "Analyze the expert dataset" )
  <*> switch (  long "algorithms"
             <> short 'A'
             <> help "Analyze the algorithm dataset" )
  <*> switch (  long "classical"
             <> short 'C'
             <> help "Analyze the classical dataset" )
  <*> switch (  long "folk"
             <> short 'F'
             <> help "Analyze the dutch folk dataset" )
  <*> switch (  long "random"
             <> short 'R'
             <> help "Analyze the random datasets" )
  <*> switch (  long "export"
             <> short 'X'
             <> help "Export MIDI files" )
  <*> switch (  long "progress"
             <> short 'P'
             <> help "Show progress bar" )

main :: IO ()
main = do
  op <- execParser opts
  let run = runAnalysis (export op) (progress op)
  when (classical op) $ do
    when (experts op) $
      run "docs/out/classical/experts" parseClassicExperts
    when (algorithms op) $
      run "docs/out/classical/algorithms" parseClassiclAlgo
  when (folk op) $ do
    when (experts op) $
      run "docs/out/folk/experts" parseFolkExperts
    when (algorithms op) $
      run "docs/out/folk/algorithms" parseFolkAlgo
  when (random op) $
    run "docs/out/random" parseRandom
  where
    opts :: ParserInfo Options
    opts = info (parseOpts <**> helper)
                (  fullDesc
                <> progDesc "Run analysis on the MIREX dataset"
                <> header "hs-mirex: a tool for music pattern discovery"
                )

runAnalysis :: Bool -> Bool -> FilePath -> IO [PatternGroup] -> IO ()
runAnalysis expo pr f_root parser = do
    -- Parse dataset to retrieve all pattern groups.
    allPatternGroups <- putStrLn "Parsing..." *> parser <* putStrLn "Parsed."
    -- Analyse individual pattern groups.
    cd f_root $ do
      analyses <-
        forM allPatternGroups $ \pg -> do
          an <- analysePatternGroup pr pg
          print an -- display on terminal
          renderOne pg an -- produce pie chart
          return an

      -- Combine all individual analyses and render in one chart.
      let finalAn = (combineAnalyses analyses) { name = "ALL" }
      print finalAn
      renderAll expo f_root finalAn

      -- Output in CSV format
      BL.writeFile "output.csv" $ encodeDefaultOrderedByName (finalAn:analyses)
