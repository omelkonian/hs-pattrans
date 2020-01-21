import Data.List ((\\), nub, isInfixOf)
import Data.Semigroup ((<>))
import Control.Monad (when, forM)
import Options.Applicative
import Data.Csv (encodeDefaultOrderedByName)
import qualified Data.ByteString.Lazy as BL

import Types
import Parser
import Analysis
-- import CompoAnalysis
-- import ExactAnalysis
-- import ProtoAnalysis
-- import Approx6Analysis
import Charts

-- | Command-line options.
data Options = Options { experts    :: Bool -- ^ analyze expert dataset
                       , algorithms :: Bool -- ^ analyze algorithm dataset
                       
                       , vm1 :: Bool -- ^ analyze algorithm dataset: VM1
                       , vm2 :: Bool -- ^ analyze algorithm dataset: VM2
                       , mp :: Bool -- ^ analyze algorithm dataset: MP
                       , siacf1 :: Bool -- ^ analyze algorithm dataset: SIAF1
                       , siacp :: Bool -- ^ analyze algorithm dataset: SIACP
                       , siacr :: Bool -- ^ analyze algorithm dataset: SIACR
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
      runComparison ("docs/out/classical/experts", parseClassicExperts)
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
          let anP = percentages an

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
          return (an,anP)

      let counts = map fst analyses
      let pers = map snd analyses
      -- Combine all individual analyses and render in one chart.
      let finalAn = (combineAnalyses counts) { name = "ALL" }
      let finalAnP = (percentages finalAn) { nameP = "ALL" }
      print finalAn
      renderAll expo finalAn

      -- Output in CSV format
      BL.writeFile "output.csv" $ encodeDefaultOrderedByName (finalAn:counts)

      -- probs <- percentages analyses
      BL.writeFile "outputP.csv" $ encodeDefaultOrderedByName (finalAnP:pers)
 
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
