import Control.Monad (when, forM)

import Data.List      ((\\), nub, isInfixOf)
import Data.Semigroup ((<>))

import Options.Applicative

import Types
import Parser
import Analysis
import Render

-- | Command-line options.
data Options = Options { experts    :: Bool -- ^ analyse expert dataset
                       , algorithms :: Bool -- ^ analyse algorithm dataset
                       
                       , vm1 :: Bool -- ^ analyse algorithm dataset: VM1
                       , vm2 :: Bool -- ^ analyse algorithm dataset: VM2
                       , mp :: Bool -- ^ analyse algorithm dataset: MP
                       , siacf1 :: Bool -- ^ analyse algorithm dataset: SIAF1
                       , siacp :: Bool -- ^ analyse algorithm dataset: SIACP
                       , siacr :: Bool -- ^ analyse algorithm dataset: SIACR
                       , cosia :: Bool
                       , cfp :: Bool

                       , siacf1d :: Bool
                       , siacpd :: Bool
                       , siacrd :: Bool
                       
                       , classical  :: Bool -- ^ analyze classical dataset
                       , folk       :: Bool -- ^ analyze dutch folk dataset
                       , heman      :: Bool
                       , eurovision :: Bool
                       , jazz       :: Bool
                       , bachChorales :: Bool -- analyze a subset of the kern dataset
                       , kern :: Bool -- analyze the kern dataset
                       
                       , random     :: Bool -- ^ analyze random datasets
                       , export     :: Bool -- ^ export MIDI files
                       , verify     :: Bool -- ^ whether to verify hypothesis
                       , toCompare  :: Bool -- ^ run cross-dataset comparison
                       , toPrint    :: Bool -- ^ whether to print results
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
  
  <*> switch (  long "vm1"
             <> short '1'
             <> help "Analyze the algorithm dataset: VM1" )
  <*> switch (  long "vm2"
             <> short '2'
             <> help "Analyze the algorithm dataset: VM2" )
  <*> switch (  long "mp"
             <> short '3'
             <> help "Analyze the algorithm dataset: MP" )
  <*> switch (  long "siacf1"
             <> short '4'
             <> help "Analyze the algorithm dataset: SIATECCompressF1" )
  <*> switch (  long "siacp"
             <> short '5'
             <> help "Analyze the algorithm dataset: SIATECCompressP" )
  <*> switch (  long "siacr"
             <> short '6'
             <> help "Analyze the algorithm dataset: SIATECCompressR" )
  <*> switch (  long "cosia"
             <> short '7'
             <> help "Analyze the algorithm dataset: COSIA" )
  <*> switch (  long "cfp"
             <> short '8'
             <> help "Analyze the algorithm dataset: CFP" )
  <*> switch (  long "siacf1d"
             <> short '9'
             <> help "Analyze the algorithm dataset: SIACCompressF1 -d" )
  <*> switch (  long "siacpd"
             <> short 'q'
             <> help "Analyze the algorithm dataset: SIACCompressP -d" )
  <*> switch (  long "siacrd"
             <> short 'w'
             <> help "Analyze the algorithm dataset: SIACCompressR -d" )
  
  <*> switch (  long "classical"
             <> short 'C'
             <> help "Analyze the classical dataset" )
  <*> switch (  long "folk"
             <> short 'F'
             <> help "Analyze the dutch folk dataset" )
  <*> switch (  long "heman"
             <> short 'H'
             <> help "Analyze the HEMAN dataset" )
  <*> switch (  long "eurovision"
             <> short 'o'
             <> help "Analyze the eurovision dataset" )
  <*> switch (  long "jazz"
             <> short 'j'
             <> help "Analyze the Omnibook from Klaus jazz dataset" )
  <*> switch (  long "bachChorales"
             <> short 'b'
             <> help "Analyze the bach Chorales" )
  <*> switch (  long "kern"
             <> short 'k'
             <> help "Analyze the kern dataset" )
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
  <*> switch (  long "print"
             <> short 'P'
             <> help "Whether to print results in the terminal." )

-- | Which kind of analysis to run?
currentAnalysis :: Analysis
currentAnalysis = fullAnalysis

analysePg :: PatternGroup -> IO AnalysisResult
analysePg = return . analysePatternGroup currentAnalysis

printAn :: Bool -> AnalysisResult -> IO ()
printAn toP an = if toP then print (currentAnalysis, an) else putStr "."

writeCSV :: String -> Bool -> [AnalysisResult] -> IO ()
writeCSV fname expo as = dumpAnalyses fname expo currentAnalysis as

-- | Main function.
main :: IO ()
main = do
  op <- execParser opts
  let run = runAnalysis (export op, verify op, toPrint op)
  when (classical op) $ do
    when (experts op) $
      run "docs/out/classical/experts" parseClassicExperts
    when (algorithms op) $
      run "docs/out/classical/algorithms" parseClassicAlgo
    when (vm1 op) $
      run "docs/out/classical/vm1" parseClassicAlgoVM1
    when (vm2 op) $
      run "docs/out/classical/vm2" parseClassicAlgoVM2
    when (mp op) $
      run "docs/out/classical/mp" parseClassicAlgoMP
    when (siacf1 op) $
      run "docs/out/classical/siacf1" parseClassicAlgoSIACF1
    when (siacp op) $
      run "docs/out/classical/siacp" parseClassicAlgoSIACP
    when (siacr op) $
      run "docs/out/classical/siacr" parseClassicAlgoSIACR
    when (toCompare op) $
      runComparison (export op, toPrint op)
                    ("docs/out/classical/experts", parseClassicExperts)
                    ("docs/out/classical/algorithms", parseClassicAlgo)
      
  when (folk op) $ do
    when (experts op) $
      run "docs/out/folk/experts" parseFolkExperts
    when (algorithms op) $
      run "docs/out/folk/algorithms" parseFolkAlgo
    when (vm1 op) $
      run "docs/out/folk/vm1" parseFolkAlgoVM1
    when (vm2 op) $
      run "docs/out/folk/vm2" parseFolkAlgoVM2
    when (mp op) $
      run "docs/out/folk/mp" parseFolkAlgoMP
    when (siacf1 op) $
      run "docs/out/folk/siacf1" parseFolkAlgoSIACF1
    when (siacp op) $
      run "docs/out/folk/siacp" parseFolkAlgoSIACP
    when (siacr op) $
      run "docs/out/folk/siacr" parseFolkAlgoSIACR
    when (cfp op) $
      run "docs/out/folk/cfp" parseFolkAlgoSIACFP
    when (cosia op) $
      run "docs/out/folk/cosia" parseFolkAlgoCOSIA
      
  when (heman op) $ do
    when (experts op) $
      run "docs/out/heman/annotations" parseHEMANAnnotations
      -- run "docs/out/heman/annotationsHigh" parseHEMANAnnotationsHigh
      -- run "docs/out/heman/annotationsLow" parseHEMANAnnotationsLow
    when (siacf1 op) $
      run "docs/out/heman/siacf1" parseHEMANAlgoSIACF1
    when (siacp op) $
      run "docs/out/heman/siacp" parseHEMANAlgoSIACP
    when (siacr op) $
      run "docs/out/heman/siacr" parseHEMANAlgoSIACR
    when (siacf1d op) $
      run "docs/out/heman/siacf1d" parseHEMANAlgoSIACF1D
    when (siacpd op) $
      run "docs/out/heman/siacpd" parseHEMANAlgoSIACPD
    when (siacrd op) $
      run "docs/out/heman/siacrd" parseHEMANAlgoSIACRD
      
  when (eurovision op) $ do
    when (siacf1 op) $
      run "docs/out/eurovision/siacf1" parseEuroAlgoSIACF1
    when (siacp op) $
      run "docs/out/eurovision/siacp" parseEuroAlgoSIACP
    when (siacr op) $
      run "docs/out/eurovision/siacr" parseEuroAlgoSIACR
    when (siacf1d op) $
      run "docs/out/eurovision/siacf1d" parseEuroAlgoSIACF1D
    when (siacpd op) $
      run "docs/out/eurovision/siacpd" parseEuroAlgoSIACPD
    when (siacrd op) $
      run "docs/out/eurovision/siacrd" parseEuroAlgoSIACRD
  
  when (jazz op) $ do
    when (siacf1 op) $
      run "docs/out/jazz/siacf1" parsejazzAlgoSIACF1
    when (siacp op) $
      run "docs/out/jazz/siacp" parsejazzAlgoSIACP
    when (siacr op) $
      run "docs/out/jazz/siacr" parsejazzAlgoSIACR
    when (siacf1d op) $
      run "docs/out/jazz/siacf1d" parsejazzAlgoSIACF1D
    when (siacpd op) $
      run "docs/out/jazz/siacpd" parsejazzAlgoSIACPD
    when (siacrd op) $
      run "docs/out/jazz/siacrd" parsejazzAlgoSIACRD

  when (bachChorales op) $ do
    when (siacf1 op) $
      run "docs/out/kern/bachChorales/siacf1" parsekernBachChoralesAlgoSIACF1
    when (siacp op) $
      run "docs/out/kern/bachChorales/siacp" parsekernBachChoralesAlgoSIACP
    when (siacr op) $
      run "docs/out/kern/bachChorales/siacr" parsekernBachChoralesAlgoSIACR
    when (siacf1d op) $
      run "docs/out/kern/bachChorales/siacf1d" parsekernBachChoralesAlgoSIACF1D
    when (siacpd op) $
      run "docs/out/kern/bachChorales/siacpd" parsekernBachChoralesAlgoSIACPD
    when (siacrd op) $
      run "docs/out/kern/bachChorales/siacrd" parsekernBachChoralesAlgoSIACRD
  
  when (kern op) $ do
    when (algorithms op) $
      runManyKernSIAF1Analysis "data/kerns/patterns/algs" "docs/out/kern/algorithms" parseKernAlgs
    when (toCompare op) $ do
      runComparison (export op, toPrint op)
                    ("docs/out/folk/experts", parseFolkExperts)
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

runComparison :: (Bool, Bool)
              -> (FilePath, IO [PatternGroup]) -- ^ experts
              -> (FilePath, IO [PatternGroup]) -- ^ algorithms
              -> IO ()
runComparison (expo, toP) (f_experts, parseExperts) (f_algo, parseAlgo) = do
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
        analysePg pg

      -- Aggregate results for a particular piece/alg (containing all expert prototypes)
      let finalAn = (mconcat analyses)
                    {name = "ALL(" ++ piece ++ ":" ++ alg ++ ")"}
      printAn toP finalAn

      -- Output in CSV format
      let f_root = f_algo ++ "/" ++ piece ++ "/" ++ alg
      cd f_root $
        writeCSV "comparison" expo (finalAn:analyses)
      putStrLn $ "\t\tWrote " ++ f_root ++ "/comparison.csv"
      return finalAn

    -- Aggregate results for a particular piece (containing all algorithms)
    let allAlgAnalyses = (mconcat algAnalyses)
                         {name = "ALL(" ++ piece ++ ")"}
    let f_root = f_algo ++ "/" ++ piece
    cd f_root $
      writeCSV "comparison" expo [allAlgAnalyses]
    putStrLn $ "\tWrote " ++ f_root ++ "/comparison.csv"

    return algAnalyses

  -- Aggregate results for a particular algorithm (containing all pieces)
  let pieceAnalyses = concat pieceAnalyses'
  algAnalyses <- forM algs $ \alg -> do
    let algPieceAnalyses = filter (isInfixOf alg . name) pieceAnalyses
    let algAn = (mconcat algPieceAnalyses) {name = "ALL(" ++ alg ++ ")"}
    cd f_algo $
      writeCSV alg expo [algAn]
    putStrLn $ "Wrote " ++ f_algo ++ "/" ++ alg
    return algAn

  -- Aggregate all results (coming from piece aggregations)
  cd f_algo $
    writeCSV "comparison" expo [(mconcat pieceAnalyses) {name = "ALL"}]
  putStrLn $ "\tWrote " ++ f_algo ++ "/comparison.csv"

  -- Aggregate all results (coming from algorithm aggregations)
  cd f_algo $
    writeCSV "comparisonA" expo [(mconcat algAnalyses) {name = "ALL"}]
  putStrLn $ "\tWrote " ++ f_algo ++ "/comparisonA.csv"


-- Analyse given music pattern dataset.
runAnalysis :: (Bool, Bool, Bool) -> FilePath -> IO [PatternGroup] -> IO ()
runAnalysis (expo, ver, toP) f_root parser = do
    -- Parse dataset to retrieve all pattern groups.
    putStrLn $ "Parsing " ++ f_root ++ "..."
    allPatternGroups <- filter (not . null . patterns) <$> parser
    putStrLn "Parsed."
    -- Analyse individual pattern groups.
    cd f_root $ do
      analyses <-
        forM allPatternGroups $ \pg -> do
          an <- analysePg pg

          -- putStrLn (name an)
          printAn toP an -- display on terminal

          -- Verify (hope to be slow)
          when ver $ do
            let uns = snd <$> unclassified an
            let tot = length uns
            when (tot > 0) $ do
              uns' <- verifyEquivClassHypothesis uns (patterns pg \\ uns)
              putStrLn $ "Verified (" ++ show uns' ++ " / " ++ show tot ++ ")"


          renderOne currentAnalysis pg an -- produce pie chart
          return an

      -- Combine all individual analyses and render in one chart.
      let finalAn = (mconcat analyses) { name = "ALL" }
      printAn toP finalAn
      render currentAnalysis "ALL" finalAn

      -- Output in CSV format
      writeCSV "output" expo (finalAn:analyses)
 
runManyKernSIAF1Analysis :: FilePath -> FilePath -> (FilePath -> IO [PatternGroup]) -> IO ()
runManyKernSIAF1Analysis input_root f_root parser = cd input_root $ do
  flist <- listDirs
  let inputdirs = map (++ "/tlf1d") flist
  let parsed = map parser inputdirs
  let outputdirs = map ((f_root++"/")++) flist
  
  sequence_ (zipWith (runAnalysis (False, False)) outputdirs parsed)

-- | Verify the hypothesis that our transformations form equivalence classes.
-- This is done by trying out other patterns in the group as base patterns.
verifyEquivClassHypothesis :: [Pattern] -- ^ unclassified patterns
                           -> [Pattern] -- ^ possible bases
                           -> IO Int
verifyEquivClassHypothesis []   _           = return 0
verifyEquivClassHypothesis uns []           = return (length uns)
verifyEquivClassHypothesis uns (base:bases) = do
  an <- analysePg (PatternGroup "" "" "" base uns)
  let uns' = snd <$> unclassified an
  verifyEquivClassHypothesis uns' bases
