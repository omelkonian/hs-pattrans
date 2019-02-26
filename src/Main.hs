import Data.List ((\\), nub)
import Data.Semigroup ((<>))
import Control.Monad (when, forM, forM_)
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
                       , toCompare  :: Bool -- ^ run cross-dataset comparison
                       }

-- | Parsing command-line options.
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
  <*> switch (  long "compare"
             <> short 'M'
             <> help "Compare expert annotations and algorithmic output" )

-- | Main function.
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
    when (toCompare op) $ do
      runComparison

  when (random op) $
    run "docs/out/random" parseRandom

  where
    opts :: ParserInfo Options
    opts = info (parseOpts <**> helper)
                (  fullDesc
                <> progDesc "Run analysis on the MIREX dataset"
                <> header "hs-mirex: a tool for music pattern discovery"
                )

runComparison :: IO ()
runComparison = do
  -- parse expert annotations
  putStrLn $ "Parsing docs/out/folk/experts..."
  pgsE <- filter (not . null . patterns) <$> parseFolkExperts
  putStrLn "Parsed."

  -- parse algorithmic output
  putStrLn $ "Parsing docs/out/folk/algorithms..."
  pgsA <- filter (not . null . patterns) <$> parseFolkAlgo
  putStrLn "Parsed."

  let algs   = nub (expert_name <$> pgsA)
  let pieces = nub (piece_name  <$> pgsA)

  -- for each song
  forM_ pieces $ \piece -> do
    let pgsE' = filter ((== piece) . piece_name) pgsE
    let expertPrs = basePattern <$> pgsE'

    -- for each algorithm
    forM_ algs $ \alg -> do
      let pgsA' = filter (\pg -> (piece_name  pg == piece)
                              && (expert_name pg == alg )) pgsA
      let algPrototypes = basePattern <$> pgsA'

      -- for each expert prototype
      analyses <- forM expertPrs $ \expertPrototype -> do
        -- create a pattern group for analysis
        let pg = PatternGroup { piece_name   = piece
                              , expert_name  = alg
                              , pattern_name = "-"
                              , basePattern  = expertPrototype
                              , patterns     = algPrototypes
                              }
        analysePatternGroup False pg

      let finalAn = (combineAnalyses analyses) { name = "ALL" }
      -- print finalAn

      -- Output in CSV format
      let f_root = "docs/out/folk/algorithms/" ++ piece ++ "/" ++ alg
      cd f_root $
        BL.writeFile "comparison.csv" $
          encodeDefaultOrderedByName (finalAn:analyses)
      putStrLn $ "Wrote " ++ f_root ++ "/comparison.csv"

-- Analyse given music pattern dataset.
runAnalysis :: (Bool, Bool, Bool) -> FilePath -> IO [PatternGroup] -> IO ()
runAnalysis (expo, pr, ver) f_root parser = do
    -- Parse dataset to retrieve all pattern groups.
    putStrLn $ "Parsing " ++ f_root ++ "..."
    allPatternGroups <- filter (not . null . patterns) <$> parser
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
verifyEquivClassHypothesis []   _           = return 0
verifyEquivClassHypothesis uns []           = return (length uns)
verifyEquivClassHypothesis uns (base:bases) = do
  an <- analysePatternGroup False (PatternGroup "" "" "" base uns)
  let uns' = snd <$> unclassified an
  verifyEquivClassHypothesis uns' bases
