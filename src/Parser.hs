{-# LANGUAGE ExistentialQuantification #-}
module Parser where

import Control.Monad (forM, mapM_, filterM, void)
import Data.List (sort, isInfixOf, sortOn, groupBy)
import System.Directory

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tokens
import qualified Data.Char as Char

import Types
import MIDI (readFromMidi)
  
-------------------------
-- Music parsers.

parseConcatMidMusics :: IO MusicPiece
parseConcatMidMusics = cd "data/queryDataset/" $ do
  f_music <- listFiles
  musicMidis <- foldMap readFromMidi f_music
  return musicMidis

parseMidMusics :: IO ([MusicPiece], [FilePath])
parseMidMusics = cd "data/queryDataset/" $ do
  f_music <- listFiles
  musicMidis <- traverse readFromMidi f_music
  return (musicMidis, f_music)

-------------------------
-- Parser utilities.

parseAlgoPiece :: (String -> String) -> String -> IO [PatternGroup]
parseAlgoPiece sanitize f_alg = listFiles >>= ((concat <$>) . pmapM parseFile)
  where
    parseFile fname = parseMany (patternGroupP (sanitize fname) f_alg) fname

patternGroupP :: String -> String -> Parser PatternGroup
patternGroupP piece_n algo_n =
  PatternGroup piece_n algo_n <$> nameP 'p'
                              <*> patternP
                              <*> many patternP
  where
    patternP :: Parser Pattern
    patternP = nameP 'o' *> many noteP
    nameP :: Char -> Parser String
    nameP c = char c *> ((:) <$> return c <*> many1 alphaNum) <* lineP

-- | Parse one entry from a MIREX piece of music.
mirexP :: Parser Note
mirexP = Note <$> (floatP <* sepP) <*> (intP <* sepP)
               <* (intP <* sepP) <* (floatP <* sepP)
               <* intP <* newline

noteP :: Parser Note
noteP = Note <$> (floatP <* sepP)
             <*> intP <* lineP

sepP :: Parser String
sepP = string ", "

intP :: Parser Integer
intP = Tokens.integer haskell
         <* optional (string "." <* many (string "0") <* optional (string " "))

floatP :: Parser Double
floatP = negP <|> Tokens.float haskell
  where negP = (\i -> -i) <$> (string "-" *> Tokens.float haskell)

lineP :: Parser ()
lineP = void (newline <|> crlf) <|> void (many1 space)

parseMany :: Parser a -> FilePath -> IO [a]
parseMany p f = do
  input <- readFile f
  case runParser ((many p <* many lineP) <* eof) () f input of
    Left err -> error $ show err
    Right x  -> return x

-------------------------
-- File-system utilities.

cd :: FilePath -> IO a -> IO a
cd fpath c = createDirectoryIfMissing True fpath
          >> withCurrentDirectory fpath c

listDirs :: IO [FilePath]
listDirs = sort <$> (getCurrentDirectory
                >>= listDirectory
                >>= filterM doesDirectoryExist)

listFiles :: IO [FilePath]
listFiles = sort <$> (getCurrentDirectory
                 >>= listDirectory
                 >>= filterM ((not <$>) . doesDirectoryExist))

listFilesRecursively :: IO [FilePath]
listFilesRecursively = do
  dirs <- listDirs
  fs   <- listFiles
  fss  <- forM dirs $ \d -> cd d $ fmap (\f -> d ++ "/" ++ f) <$> listFilesRecursively
  return (fs ++ concat fss)

emptyDirectory :: FilePath -> IO ()
emptyDirectory f_root = cd f_root $ mapM_ removeFile =<< listFiles
          
-------------------------
-- Datasets.

data Dataset = Dataset
  { datasetName   :: FilePath
  , parseExperts  :: IO [PatternGroup]
  , parseAlgo     :: IO [PatternGroup]
  , parseNgram    :: IO [PatternGroup]
  , parseRandom   :: IO [PatternGroup]
  -- | Parse a music piece.
  , parsePiece    :: Song -> IO MusicPiece
  -- | Normalize names of musical pieces to a static representation.
  , sanitizeSong  :: String -> String
  }

noop :: IO [a]
noop = return []

defDataset :: Dataset
defDataset = Dataset "default" noop noop noop noop (const noop) id

classical :: Dataset
classical = Dataset "classical" pExp pAlgo noop noop pPiece sanitize
  where
    pExp = cd "dataset/classical/experts" $ do
      f_roots <- listDirs
      res <- forM f_roots $ \f_root -> cd (f_root ++ "/monophonic/repeatedPatterns") $ do
        f_patExs <- listDirs
        allPats <- forM f_patExs $ \f_patEx -> cd f_patEx $ do
          f_patTys <- listDirs
          forM f_patTys $ \f_patTy -> do
            basePat:pats <- cd (f_patTy ++ "/occurrences/csv") $ do
              f_pats <- listFiles
              pforM f_pats (parseMany noteP)
            return $ PatternGroup { piece_name   = f_root
                                  , expert_name  = f_patEx
                                  , pattern_name = f_patTy
                                  , basePattern  = basePat
                                  , patterns     = pats }
        return $ concat allPats
      return $ concat res

    pAlgo = cd "dataset/classical/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ do
        f_versions <- listDirs
        if null f_versions then
          parseAlgoPiece sanitize f_alg
        else
          concat <$> forM f_versions
            (\f_v -> cd f_v $ parseAlgoPiece sanitize (f_alg ++ "-" ++ f_v))
      return (concat allPgs)

    pPiece song = cd ("dataset/classical/experts/" ++ sanitize song ++ "/monophonic/csv") $ do
      [f_music] <- listFiles
      parseMany mirexP f_music
  
    sanitize s 
      | (("bach" `isInfixOf` s) || ("wtc" `isInfixOf` s))
          && not ("1" `isInfixOf` s) && not ("2" `isInfixOf` s)
      = "bachBWV889Fg"
      | ("beethoven" `isInfixOf` s) || ("sonata01" `isInfixOf` s)
      = "beethovenOp2No1Mvt3"
      | ("chopin" `isInfixOf` s) || ("mazurka" `isInfixOf` s)
      = "chopinOp24No4"
      | ("gibbons" `isInfixOf` s) || ("silver" `isInfixOf` s)
      = "gibbonsSilverSwan1612"
      | ("mozart" `isInfixOf` s) || ("sonata04" `isInfixOf` s)
      = "mozartK282Mvt2"
      | otherwise
      = s

folk :: Dataset
folk = Dataset "folk" pExp pAlgo pRandom noop (const noop) sanitize
  where
    pExp = cd "dataset/folk/patterns/expert" $ do
      allPgs <- parseAlgoPiece sanitize "exp"
      return (groupPatterns allPgs)
      where
        groupPatterns :: [PatternGroup] -> [PatternGroup]
        groupPatterns = (foldl1 combinePatterns <$>)
                      . groupBy samePattern
                      . sortOn show

        samePattern :: PatternGroup -> PatternGroup -> Bool
        samePattern (PatternGroup p e pa _ _) (PatternGroup p' e' pa' _ _) =
          p == p' && e == e' && pa == pa'

        combinePatterns :: PatternGroup -> PatternGroup -> PatternGroup
        combinePatterns p1@(PatternGroup p e pa b os) p2@(PatternGroup _ _ _ b' os')
          | samePattern p1 p2 = PatternGroup p e pa b $ (b' : os) ++ os'
          | otherwise         = error "Cannot combine occurences of different patterns"

    pAlgo = cd "dataset/folk/patterns/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ parseAlgoPiece sanitize f_alg
      return (concat allPgs)

    pRandom = cd "dataset/folk/ranexcerpts" $ do
      f_groups <- listDirs
      allPgs <- forM f_groups $ \f_group -> cd f_group $ do
        fs <- listFiles
        let families = groupBy (\x y -> sanitize x == sanitize y) $ sortOn sanitize fs
        forM families $ \family -> do
          (base:pats) <- pmapM readFromMidi family -- convert MIDI to Pattern
          return PatternGroup { piece_name   = sanitize (head family)
                              , expert_name  = "RAND"
                              , pattern_name = f_group
                              , basePattern  = base
                              , patterns     = pats }
      return (concat allPgs)
  
    sanitize s 
      | ("Daar_g" `isInfixOf` s)        = "DaarGingEenHeer"
      | ("Daar_r" `isInfixOf` s)        = "DaarReedEenJonkheer"
      | ("Daar_w" `isInfixOf` s)        = "DaarWasLaatstmaalEenRuiter"
      | ("Daar_z" `isInfixOf` s)        = "DaarZouErEenMaagdjeVroegOpstaan"
      | ("Een_l" `isInfixOf` s)         = "EenLindeboomStondInHetDal"
      | ("Een_S" `isInfixOf` s)         = "EenSoudaanHadEenDochtertje"
      | ("En" `isInfixOf` s)            = "EnErWarenEensTweeZoeteliefjes"
      | ("Er_r" `isInfixOf` s)          = "ErReedErEensEenRuiter"
      | ("Er_was_een_h" `isInfixOf` s)  = "ErWasEenHerderinnetje"
      | ("Er_was_een_k" `isInfixOf` s)  = "ErWasEenKoopmanRijkEnMachtig"
      | ("Er_was_een_m" `isInfixOf` s)  = "ErWasEenMeisjeVanZestienJaren"
      | ("Er_woonde" `isInfixOf` s)     = "ErWoondeEenVrouwtjeAlOverHetBos"
      | ("Femmes" `isInfixOf` s)        = "FemmesVoulezVousEprouver"
      | ("Heer_Halewijn" `isInfixOf` s) = "HeerHalewijn"
      | ("Het_v" `isInfixOf` s)         = "HetVrouwtjeVanStavoren"
      | ("Het_was_l" `isInfixOf` s)     = "HetWasLaatstOpEenZomerdag"
      | ("Het_was_o" `isInfixOf` s)     = "HetWasOpEenDriekoningenavond"
      | ("Ik" `isInfixOf` s)            = "IkKwamLaatstEensInDeStad"
      | ("Kom" `isInfixOf` s)           = "KomLaatOnsNuZoStilNietZijn"
      | ("Lieve" `isInfixOf` s)         = "LieveSchipperVaarMeOver"
      | ("O_God" `isInfixOf` s)         = "OGodIkLeefInNood"
      | ("Soldaat" `isInfixOf` s)       = "SoldaatKwamUitDeOorlog"
      | ("Vaarwel" `isInfixOf` s)       = "VaarwelBruidjeSchoon"
      | ("Wat" `isInfixOf` s)           = "WatZagIkDaarVanVerre"
      | ("Zolang" `isInfixOf` s)        = "ZolangDeBoomZalBloeien"
      | otherwise                       = s

heman :: Dataset
heman = Dataset "heman" pExp pAlgo pNgram noop pPiece sanitize
  where
    pExp = cd "dataset/HEMAN/patterns/annotations/" $ do
      algPgs <- parseAlgoPiece sanitize "Human"
      return algPgs

    pAlgo = cd "dataset/HEMAN/patterns/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ parseAlgoPiece sanitize f_alg
      return (concat allPgs)

    pNgram = cd "dataset/HEMAN/patterns/ngram/" $ do
      algPgs <- parseAlgoPiece sanitize "Ngrams"
      return algPgs
      
    pPiece song = cd ("dataset/HEMAN/piece/csv" ++ sanitize song) $ do
      [f_music] <- listFiles
      parseMany mirexP f_music
  
    sanitize s 
      | ("bach1" `isInfixOf` s) = "bach1"
      | ("bach2" `isInfixOf` s) = "bach2"
      | ("bee1" `isInfixOf` s)  = "bee1"
      | ("mo155" `isInfixOf` s) = "mo155"
      | ("mo458" `isInfixOf` s) = "mo458"
      | otherwise               = s

queries :: Dataset
queries = defDataset
  { datasetName = "queries"
  , parsePiece  = \s -> cd "dataset/queryDataset/" $ do
      f_music <- listFiles
      -- putStrLn $ "Searching: " ++ show f_music
      let matched = (filter (`isInfixOf` s)) f_music
      putStrLn $ "Matched: " ++ show matched
      musicMidis <- foldMap readFromMidi matched
      return musicMidis
  }

qsynth :: Dataset
qsynth = defDataset
  { datasetName = "synth"
  , parsePiece  = \_ -> cd "dataset/querySynth/" $ do
      f_music <- listFiles
      musicMidis <- foldMap readFromMidi f_music
      return musicMidis
  }

eurovision :: Dataset
eurovision = defDataset
  { datasetName = "eurovision"
  , parseAlgo  = cd "dataset/eurovision/patterns/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ parseAlgoPiece id f_alg
      return (concat allPgs)
  }


eurovisionG :: Dataset
eurovisionG = defDataset
  { datasetName = "eurovisionG"
  , parseAlgo  = cd "dataset/eurovisionG/patterns/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ parseAlgoPiece id f_alg
      return (concat allPgs)
  }

eurovisionU :: Dataset
eurovisionU = defDataset
  { datasetName = "eurovisionU"
  , parseAlgo  = cd "dataset/eurovisionU/patterns/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ parseAlgoPiece id f_alg
      return (concat allPgs)
  }

synth :: Dataset
synth = defDataset
  { datasetName = "synth"
  , parseAlgo  = cd "dataset/synth/patterns/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ parseAlgoPiece id f_alg
      return (concat allPgs)
  }


jazz :: Dataset
jazz = defDataset
  { datasetName = "jazz"
  , parseAlgo  = cd "dataset/jazz/patterns/alg" $ do
      f_algs <- listDirs
      allPgs <- forM f_algs $ \f_alg -> cd f_alg $ parseAlgoPiece id f_alg
      return (concat allPgs)
  }

bach371 :: Dataset
bach371 = Dataset "bach371" noop noop pNgram noop (const noop) sanitize
  where
    pNgram = cd "dataset/bach371/patterns/ngram/" $ do
      algPgs <- parseAlgoPiece sanitize "Ngrams"
      return algPgs

    sanitize = filter Char.isDigit
    
    
-- kern :: Dataset
-- kern = defDataset
--   { datasetName = "kerns"
--   , parseAlgo  = cd "data/kerns/patterns/alg" $ do
--       f_algs <- listDirs
--       allPgs <- forM f_algs $ \f_alg -> cd f_alg $ do
--         f_composers <- listDirs
--         concat <$> forM f_composers
--           (\f_c -> cd f_c $ parseAlgoPiece id f_c)
--       return (concat allPgs)
--   }

datasets :: [Dataset]
datasets = 
  [ classical
  , folk
  , heman
  , eurovision
  , jazz
  -- , kern
  , queries
  , qsynth
  , synth
  , eurovisionG
  , eurovisionU
  , bach371
  ]
