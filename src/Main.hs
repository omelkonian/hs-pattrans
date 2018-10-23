import Data.List ((\\))
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
                       , verify     :: Bool -- ^ whether to verify hypothesis
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
  <*> switch (  long "verify"
             <> short 'V'
             <> help "Verify equivalence-class hypothesis" )

main :: IO ()
main = do
  op <- execParser opts
  let run = runAnalysis (export op, progress op, verify op)
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

runAnalysis :: (Bool, Bool, Bool) -> FilePath -> IO [PatternGroup] -> IO ()
runAnalysis (expo, pr, ver) f_root parser = do
    -- Parse dataset to retrieve all pattern groups.
    putStrLn $ "Parsing " ++ f_root ++ "..."
    allPatternGroups <- parser
    putStrLn "Parsed."
    -- Analyse individual pattern groups.
    cd f_root $ do
      analyses <-
        forM allPatternGroups $ \pg -> do
          an <- analysePatternGroup pr pg

          putStrLn (name an)
          -- print an -- display on terminal

          -- Verify (hope to be slow)
          when ver $ do
            let uns = snd <$> unclassified an
            let tot = length uns
            when (tot > 0) $ do
              uns' <- verifyEquivClassHypothesis uns (patterns pg \\ uns)
              putStrLn $ "Verified (" ++ show uns' ++ " / " ++ show tot ++ ")"

          renderOne pg an -- produce pie chart
          return an

      -- Combine all individual analyses and render in one chart.
      let finalAn = (combineAnalyses analyses) { name = "ALL" }
      print finalAn
      renderAll expo f_root finalAn

      -- Output in CSV format
      BL.writeFile "output.csv" $ encodeDefaultOrderedByName (finalAn:analyses)

-- | Verify the hypothesis that our transformations form equivalence classes.
-- This is done by trying out other patterns in the group as base patterns.
verifyEquivClassHypothesis :: [Pattern] -- ^ unclassified patterns
                           -> [Pattern] -- ^ possible bases
                           -> IO Int
verifyEquivClassHypothesis []   _            = return 0
verifyEquivClassHypothesis uns []           = return (length uns)
verifyEquivClassHypothesis uns (base:bases) = do
  -- let uns = delete base uns0
  an <- analysePatternGroup False (PatternGroup "" "" "" base uns)
  let uns' = snd <$> unclassified an
  verifyEquivClassHypothesis uns' bases
