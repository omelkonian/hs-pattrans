module Parser ( ParseOption(..)
              , parseMirex, parseAlgo
              , cd, listDirs, listFiles
              ) where

import Control.Monad (forM, filterM, mapM, void)
import Data.List (sort, isPrefixOf)
import System.Directory

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Tokens

import Types

-- | Parser options (monophonic or polyphonic).
data ParseOption = Monophonic | Polyphonic
instance Show ParseOption where
  show Monophonic = "monophonic"
  show Polyphonic = "polyphonic"

-- | Get a specific piece of music and all its patterns from the MIREX dataset.
parseMirex :: ParseOption -> IO [PatternGroup]
parseMirex pOpt = cd "data/pieces" $ do
  f_roots <- listDirs
  res <- forM f_roots $ \f_root -> cd (f_root ++ "/" ++ show pOpt) $ do
    -- Parse music
    music <- cd "csv" $ do
      f_music:[] <- listFiles
      parseMany mirexP f_music

    -- Parse patterns
    patGroups <- cd "repeatedPatterns" $ do
      f_patExs <- listDirs
      allPats <- forM f_patExs $ \f_patEx -> cd f_patEx $ do
        f_patTys <- listDirs
        forM f_patTys $ \f_patTy -> do
          basePat:pats <- cd (f_patTy ++ "/occurrences/csv") $ do
            f_pats <- listFiles
            forM f_pats ((Pattern <$>) . parseMany noteP)
          return $ PatternGroup { piece_name   = f_root
                                , expert_name  = f_patEx
                                , pattern_name = f_patTy
                                , basePattern  = basePat
                                , patterns     = pats }
      return $ concat allPats
    return (music, patGroups)
  return $ concat $ snd <$> res

  where
    -- | Parse one entry from a MIREX piece of music.
    mirexP :: Parser MirexEntry
    mirexP = MirexEntry <$> (floatP <* sepP) <*> (intP   <* sepP)
                        <*> (intP   <* sepP) <*> (floatP <* sepP)
                        <*> intP <* lineP

-- | Parse all pattern groups (all algorithms/pieces) from the MIREX dataset.
parseAlgo :: IO [PatternGroup]
parseAlgo = cd "data/algOutput" $ do
  f_algs <- listDirs
  allPgs <- forM f_algs $ \f_alg -> cd f_alg $ do
    f_versions <- listDirs
    if null f_versions then do
      listFiles >>= ((concat <$>) . mapM (parseAlgoPiece f_alg))
    else
      concat <$> forM f_versions
        (\f_v -> cd f_v $
            listFiles >>= ((concat <$>) . mapM (parseAlgoPiece $ f_alg ++ ":" ++ f_v)))
  return (concat allPgs)

parseAlgoPiece :: String -> FilePath -> IO [PatternGroup]
parseAlgoPiece algo_n fname =
  parseMany (patternGroupP (sanitize fname) algo_n) fname
  where
    sanitize s
      | ("bach" `isPrefixOf` s) || ("wtc" `isPrefixOf` s)
      = "bachBMW889Fg"
      | ("beethoven" `isPrefixOf` s) || ("sonata01" `isPrefixOf` s)
      = "beethovenOp2No1Mvt3"
      | ("chopin" `isPrefixOf` s) || ("mazurka" `isPrefixOf` s)
      = "chopinOp24No4"
      | ("gibbons" `isPrefixOf` s) || ("silver" `isPrefixOf` s)
      = "gibbonsSilverSwan1612"
      | ("mozart" `isPrefixOf` s) || ("sonata04" `isPrefixOf` s)
      = "mozartK282Mvt2"
      | otherwise
      = s

patternGroupP :: String -> String -> Parser PatternGroup
patternGroupP piece_n algo_n =
  PatternGroup piece_n algo_n <$> nameP 'p'
                              <*> patternP
                              <*> many patternP
  where
    patternP :: Parser Pattern
    patternP = nameP 'o' *> (Pattern <$> many noteP)
    nameP :: Char -> Parser String
    nameP c = char c *> ((:) <$> return c <*> many1 alphaNum) <* lineP

--------------------
-- Parser utilities.

parseMany :: Parser a -> FilePath -> IO [a]
parseMany p f = do
  input <- readFile f
  case runParser (many p <* eof) () f input of
    Left err -> error $ show err
    Right x  -> return x

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
lineP = void (newline <|> crlf)

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
