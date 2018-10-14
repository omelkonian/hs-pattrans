import Data.Semigroup ((<>))
import Control.Monad (forM, when)
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
                       , export     :: Bool -- ^ export MIDI files
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
  <*> switch (  long "export"
             <> short 'X'
             <> help "Export MIDI files" )

main :: IO ()
main = do
  Options {experts=e, algorithms=a, classical=c, folk=f, export=x} <- execParser opts
  when (c && e) $
    runAnalysis x "docs/out/classical/experts" parseClassicExperts
  when (c && a) $
    runAnalysis x "docs/out/classical/algorithms" parseClassicExperts
  when (f && e) $
    runAnalysis x "docs/out/folk/experts" parseFolkExperts
  when (f && a) $
    runAnalysis x "docs/out/folk/algorithms" parseFolkAlgo

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
    print $ length allPatternGroups
    -- Analyse individual pattern groups.
    cd f_root $ do
      analyses <-
        forM allPatternGroups $ \pg -> do
          let an = analysePatternGroup pg
          print an -- display on terminal
          renderOne pg an -- produce pie chart
          return an

      -- Output in CSV format
      BL.writeFile "output.csv" (encodeDefaultOrderedByName analyses)

      -- Combine all individual analyses and render in one chart.
      let finalAn = combineAnalyses analyses
      putStrLn $ "ALL " ++ show finalAn
      renderAll expo f_root finalAn
