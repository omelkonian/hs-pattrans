{-# LANGUAGE FlexibleInstances #-}
import Control.Monad (when, forM, forM_)

import Data.List       ((\\), nub, isInfixOf)
import Data.List.Split (splitOn)
import Data.Semigroup  ((<>))
import Data.String     (IsString (..))

import Options.Applicative

import Types
import Parser
import Analysis
import Render
import HTML

-- | Command-line options.
data Options = Options
  { filters    :: String -- ^ filter datasets/pieces/experts
  , analysis   :: String -- ^ which analysis to run
  , outDir     :: String -- ^ where to output generated files
  , experts    :: Bool   -- ^ analyse expert dataset
  , algorithms :: Bool   -- ^ analyse algorithm dataset
  , ng         :: Bool   -- ^ analyze random datasets
  , random     :: Bool   -- ^ analyze random datasets
  , export     :: Bool   -- ^ export MIDI files
  , html       :: Bool   -- ^ whether to produce HTML website
  , verify     :: Bool   -- ^ whether to verify hypothesis
  , toCompare  :: Bool   -- ^ run cross-dataset comparison
  , toPrint    :: Bool   -- ^ whether to print results
  }

-- | Parsing command-line options.
parseOpts :: Parser Options
parseOpts = Options
  <$> strOption (  long "filters"
                <> short 'F'
                <> help "Specify a filter for the data to analyze, e.g. -F d:classical+p:bach"
                <> value "" )
  <*> strOption (  long "analysis"
                <> short 'A'
                <> help "Specify the analysis to run, e.g. -A 'approx6'"
                <> value "default" )
  <*> strOption (  long "outDir"
                <> short 'O'
                <> help "Specify an output directory to pull generated files"
                <> value "out" )
  <*> switch (  long "experts"
             <> short 'E'
             <> help "Analyze the expert dataset" )
  <*> switch (  long "algorithms"
             <> short 'L'
             <> help "Analyze the algorithm dataset" )
  <*> switch (  long "ngrams"
             <> short 'N'
             <> help "Analyze the ngram dataset" )
  <*> switch (  long "random"
             <> short 'R'
             <> help "Analyze the random datasets" )
  <*> switch (  long "export"
             <> short 'X'
             <> help "Export MIDI files" )
  <*> switch (  long "html"
             <> short 'H'
             <> help "Produce navigatable HTML." )
  <*> switch (  long "verify"
             <> short 'V'
             <> help "Verify equivalence-class hypothesis" )
  <*> switch (  long "compare"
             <> short 'M'
             <> help "Compare expert annotations and algorithmic output" )
  <*> switch (  long "print"
             <> short 'P'
             <> help "Whether to print results in the terminal." )

-- | Main function.
main :: IO ()
main = do
  op <- execParser $
    info (parseOpts <**> helper)
         ( fullDesc
        <> progDesc "Run analysis on the MIREX dataset"
        <> header "hs-mirex: a tool for music pattern discovery" )
  let
    (fd, fp, fe) = interpret $ fromString $ filters op

    currentAnalysis :: Analysis
    currentAnalysis 
      | ((_, an):_) <- filter ((analysis op `isInfixOf`) . fst) analyses
      = an
      | otherwise
      = error $ "analysis '" ++ analysis op ++ "' not found"

    analysePg :: PatternGroup -> AnalysisResult
    analysePg = analysePatternGroup currentAnalysis

    printAn :: AnalysisResult -> IO ()
    printAn an = if (toPrint op) then print (currentAnalysis, an) else putStr "."

    writeCSV :: FilePath -> [AnalysisResult] -> IO ()
    writeCSV fname = dumpAnalyses fname (export op) currentAnalysis

    filterPG :: PatternGroup -> Bool
    filterPG (PatternGroup {piece_name = p, expert_name = e, patterns = ps})
      = fp p && fe e && not (null ps)

    -- Analyse given music pattern dataset.
    runSingle :: FilePath -> IO [PatternGroup] -> IO ()
    runSingle f_root parser = do
      -- Parse dataset to retrieve all pattern groups.
      putStrLn $ "Parsing " ++ f_root ++ "..."
      allPatternGroups <- filter filterPG <$> parser
      putStrLn "Parsed."
      -- Analyse individual pattern groups.
      cd f_root $ do
        ans <-
          forM allPatternGroups $ \pg -> do
            let an = analysePg pg

            -- putStrLn (name an)
            printAn an -- display on terminal


            -- Verify (hope to be slow)
            when (verify op) $ do
              let uns  = snd <$> unclassified an
                  uns' = verifyEquivClassHypothesis uns (patterns pg \\ uns)
              putStrLn $
                "Verified (" ++ show uns' ++ " / " ++ show (length uns) ++ ")"
              
            renderOne currentAnalysis pg an -- produce pie chart
            return an

        -- Combine all individual analyses and render in one chart.
        let finalAn = (mconcat ans) { name = "ALL" }
        printAn finalAn
        render currentAnalysis "ALL" finalAn

        -- Output in CSV format
        writeCSV "output" (finalAn:ans)
     

    runComparison :: (FilePath, IO [PatternGroup]) -- ^ experts
                  -> (FilePath, IO [PatternGroup]) -- ^ algorithms
                  -> IO ()
    runComparison (f_experts, pExp) (f_algo, pAlgo) = do
      -- parse expert annotations
      putStrLn $ "Parsing " ++ f_experts ++ "..."
      pgsE <- filter filterPG <$> pExp
      putStrLn "Parsed."

      -- parse algorithmic output
      putStrLn $ "Parsing " ++ f_algo ++ "..."
      pgsA <- filter filterPG <$> pAlgo
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
          ans <- forM expertPrs $ \expertPrototype -> do
            -- create a pattern group for analysis
            let pg = PatternGroup { piece_name   = piece
                                  , expert_name  = alg
                                  , pattern_name = "-"
                                  , basePattern  = expertPrototype
                                  , patterns     = algPrototypes
                                  }
            return $ analysePg pg

          -- Aggregate results for a particular piece/alg (containing all expert prototypes)
          let finalAn = (mconcat ans)
                        {name = "ALL(" ++ piece ++ "-" ++ alg ++ ")"}
          printAn finalAn

          -- Output in CSV format
          let f_root = f_algo ++ "/" ++ piece ++ "/" ++ alg
          cd f_root $
            writeCSV "comparison" (finalAn:ans)
          putStrLn $ "\t\tWrote " ++ f_root ++ "/comparison.csv"
          return finalAn

        -- Aggregate results for a particular piece (containing all algorithms)
        let allAlgAnalyses = (mconcat algAnalyses)
                             {name = "ALL(" ++ piece ++ ")"}
        let f_root = f_algo ++ "/" ++ piece
        cd f_root $
          writeCSV "comparison" [allAlgAnalyses]
        putStrLn $ "\tWrote " ++ f_root ++ "/comparison.csv"

        return algAnalyses

      -- Aggregate results for a particular algorithm (containing all pieces)
      let pieceAnalyses = concat pieceAnalyses'
      algAnalyses <- forM algs $ \alg -> do
        let algPieceAnalyses = filter (isInfixOf alg . name) pieceAnalyses
        let algAn = (mconcat algPieceAnalyses) {name = "ALL(" ++ alg ++ ")"}
        cd f_algo $
          writeCSV alg [algAn]
        putStrLn $ "Wrote " ++ f_algo ++ "/" ++ alg
        return algAn

      -- Aggregate all results (coming from piece aggregations)
      cd f_algo $
        writeCSV "comparison" [(mconcat pieceAnalyses) {name = "ALL"}]
      putStrLn $ "\tWrote " ++ f_algo ++ "/comparison.csv"

      -- Aggregate all results (coming from algorithm aggregations)
      cd f_algo $
        writeCSV "comparisonA" [(mconcat algAnalyses) {name = "ALL"}]
      putStrLn $ "\tWrote " ++ f_algo ++ "/comparisonA.csv"

    -- | Verify the hypothesis that our transformations form equivalence classes.
    -- This is done by trying out other patterns in the group as base patterns.
    verifyEquivClassHypothesis :: [Pattern] -- ^ unclassified patterns
                               -> [Pattern] -- ^ possible bases
                               -> Int
    verifyEquivClassHypothesis [] _      = 0
    verifyEquivClassHypothesis xs []     = length xs
    verifyEquivClassHypothesis xs (b:bs) = verifyEquivClassHypothesis xs' bs
      where xs' = snd <$> unclassified (analysePg (PatternGroup "" "" "" b xs))

  -- for each dataset
  forM_ (filter (fd . datasetName) datasets) $
    \(Dataset n pExp pAlgo pNgram pRand _ _) -> do
      let path = outDir op ++ "/" ++ n ++ "/"
      when (experts op)    $ runSingle (path ++ "experts")    pExp
      when (algorithms op) $ runSingle (path ++ "algorithms") pAlgo
      when (ng op)         $ runSingle (path ++ "ngram")      pNgram
      when (random op)     $ runSingle (path ++ "random")     pRand
      when (toCompare op)  $ runComparison (path ++ "experts",    pExp)
                                           (path ++ "algorithms", pAlgo)

  -- Generate HTML
  when (html op) $
    generateAll (outDir op)

-------------------
-- Filters.

data Filter
  = DatasetF   String
  | PieceF     String
  | ExpertF    String

instance IsString Filter where
  fromString ('d':':':s) = DatasetF s
  fromString ('p':':':s) = PieceF   s
  fromString ('e':':':s) = ExpertF  s
  fromString _           = error "not a filter"

instance {-# OVERLAPPING #-} IsString [Filter] where
  fromString s = case splitOn "+" s of
    [""] -> []
    fs   -> map fromString fs

interpret :: [Filter]
          -> ( String -> Bool -- filter on datasets
             , String -> Bool -- filter on pieces
             , String -> Bool -- filter on experts/algorithms
             )
interpret fs0 = let (fd, fp, fe) = go fs0 in (mf fd, mf fp, mf fe)
  where    
    mf :: Maybe (String -> Bool) -> (String -> Bool)
    mf Nothing  = const True
    mf (Just f) = f

    (\/) :: Maybe (String -> Bool) -> (String -> Bool) -> Maybe (String -> Bool)
    Nothing \/ g = Just g
    Just f  \/ g = Just $ \x -> f x || g x

    go []     = (Nothing, Nothing, Nothing)
    go (f:fs) = 
      let (fd, fp, fe) = go fs in
      case f of
        DatasetF   s -> (fd \/ (s `isInfixOf`), fp, fe)
        PieceF     s -> (fd, fp \/ (s `isInfixOf`), fe)
        ExpertF    s -> (fd, fp, fe \/ (s `isInfixOf`))

-- runManyKernSIAF1Analysis :: FilePath -> FilePath -> (FilePath -> IO [PatternGroup]) -> IO ()
-- runManyKernSIAF1Analysis input_root f_root parser = cd input_root $ do
--   flist <- listDirs
--   let inputdirs = map (++ "/tlf1d") flist
--   let parsed = map parser inputdirs
--   let outputdirs = map ((f_root++"/")++) flist
  
--   sequence_ (zipWith (runAnalysis (False, False, False)) outputdirs parsed)
