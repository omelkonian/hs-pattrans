import Data.List ((\\), nub, elemIndices, isInfixOf)
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
  let run = runAnalysis (export op, verify op)
  when (classical op) $ do
    when (experts op) $
      run "docs/out/classical/experts" parseClassicExperts
    when (algorithms op) $
      run "docs/out/classical/algorithms" parseClassicAlgo
    when (toCompare op) $
      runComparison ("docs/out/classical/experts", parseClassicExperts)
                    ("docs/out/classical/algorithms", parseClassicAlgo)
  when (folk op) $ do
    when (experts op) $
      run "docs/out/folk/experts" parseFolkExperts
    when (algorithms op) $
      run "docs/out/folk/algorithms" parseFolkAlgo
    when (toCompare op) $ do
      runComparison ("docs/out/folk/experts", parseFolkExperts)
                    ("docs/out/folk/algorithms", parseFolkAlgo)


  when (random op) $
    run "docs/out/random" parseRandom

  where
    opts :: ParserInfo Options
    opts = info (parseOpts <**> helper)
                (  fullDesc
                <> progDesc "Run analysis on the MIREX dataset"
                <> header "hs-mirex: a tool for music pattern discovery"
                )

runComparison :: (FilePath, IO [PatternGroup]) -- ^ experts
              -> (FilePath, IO [PatternGroup]) -- ^ algorithms
              -> IO ()
runComparison (f_experts, parseExperts) (f_algo, parseAlgo) = do
  -- parse expert annotations
  putStrLn $ "Parsing " ++ f_experts ++ "..."
  pgsE <- filter (not . null . patterns) <$> parseExperts
  putStrLn "Parsed."

  -- parse algorithmic output
  putStrLn $ "Parsing " ++ f_algo ++ "..."
  pgsA <- filter (not . null . patterns) <$> parseAlgo
  putStrLn "Parsed."

  let algs   = nub (expert_name <$> pgsA)
  let pieces = nub (piece_name  <$> pgsA)

  -- for each song
  pieceAnalyses' <- forM pieces $ \piece -> do
    let pgsE' = filter ((== piece) . piece_name) pgsE
    let expertPrs = basePattern <$> pgsE'

    -- for each algorithm
    algAnalyses <- forM algs $ \alg -> do
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
        analysePatternGroup pg

      -- Aggregate results for a particular piece/alg (containing all expert prototypes)
      let finalAn = (combineAnalyses analyses)
                    {name = "ALL(" ++ piece ++ ":" ++ alg ++ ")"}
      -- print finalAn

      -- Output in CSV format
      let f_root = f_algo ++ "/" ++ piece ++ "/" ++ alg
      cd f_root $
        BL.writeFile "comparison.csv" $
          encodeDefaultOrderedByName (finalAn:analyses)
      putStrLn $ "\t\tWrote " ++ f_root ++ "/comparison.csv"
      return finalAn

    -- Aggregate results for a particular piece (containing all algorithms)
    let allAlgAnalyses = (combineAnalyses algAnalyses)
                         {name = "ALL(" ++ piece ++ ")"}
    let f_root = f_algo ++ "/" ++ piece
    cd f_root $
      BL.writeFile "comparison.csv" $
        encodeDefaultOrderedByName [allAlgAnalyses]
    putStrLn $ "\tWrote " ++ f_root ++ "/comparison.csv"

    return algAnalyses

  -- Aggregate results for a particular algorithm (containing all pieces)
  let pieceAnalyses = concat pieceAnalyses'
  algAnalyses <- forM algs $ \alg -> do
    let algPieceAnalyses = filter (isInfixOf alg . name) pieceAnalyses
    let algAn = (combineAnalyses algPieceAnalyses) {name = "ALL(" ++ alg ++ ")"}
    let f_name = alg ++ ".csv"
    cd f_algo $
      BL.writeFile f_name $
        encodeDefaultOrderedByName [algAn]
    putStrLn $ "Wrote " ++ f_algo ++ "/" ++ f_name
    return algAn

  -- Aggregate all results (coming from piece aggregations)
  cd f_algo $
    BL.writeFile "comparison.csv" $
      encodeDefaultOrderedByName [(combineAnalyses pieceAnalyses) {name = "ALL"}]
  putStrLn $ "\tWrote " ++ f_algo ++ "/comparison.csv"

  -- Aggregate all results (coming from algorithm aggregations)
  cd f_algo $
    BL.writeFile "comparisonA.csv" $
      encodeDefaultOrderedByName [(combineAnalyses algAnalyses) {name = "ALL"}]
  putStrLn $ "\tWrote " ++ f_algo ++ "/comparisonA.csv"

-- Analyse given music pattern dataset.
runAnalysis :: (Bool, Bool) -> FilePath -> IO [PatternGroup] -> IO ()
runAnalysis (expo, ver) f_root parser = do
    -- Parse dataset to retrieve all pattern groups.
    putStrLn $ "Parsing " ++ f_root ++ "..."
    allPatternGroups <- filter (not . null . patterns) <$> parser
    putStrLn "Parsed."
    -- Analyse individual pattern groups.
    cd f_root $ do
      analyses <-
        forM allPatternGroups $ \pg -> do
          an <- analysePatternGroup pg

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
      renderAll expo finalAn

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
  an <- analysePatternGroup (PatternGroup "" "" "" base uns)
  let uns' = snd <$> unclassified an
  verifyEquivClassHypothesis uns' bases
