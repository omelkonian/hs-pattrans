module Parser ( ParseOption(..)
              , parseMirex
              , cd, listDirs, listFiles
              ) where

import Control.Monad (forM, filterM)
import Data.List (sort)
import System.Directory

import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token    as Tokens

import Types

-- | Parser options (monophonic or polyphonic).
data ParseOption = Monophonic | Polyphonic
instance Show ParseOption where
  show Monophonic = "monophonic"
  show Polyphonic = "polyphonic"

-- | Get a specific piece of music and all its patterns from the MIREX dataset.
parseMirex :: ParseOption -> FilePath -> IO ([MirexEntry], [PatternGroup])
parseMirex pOpt f_root = cd (f_root ++ "/" ++ show pOpt) $ do
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

-- | Parse one entry from a MIREX piece of music.
mirexP :: Parser MirexEntry
mirexP = MirexEntry <$> (floatP <* sepP) <*> (intP   <* sepP)
                    <*> (intP   <* sepP) <*> (floatP <* sepP)
                    <*> intP <* newline


-- | Parse one entry from a MIREX pattern.
noteP :: Parser Note
noteP = Note <$> (floatP <* sepP)
             <*> intP <* newline

--------------------
-- Parser utilities.

parseMany :: Parser a -> FilePath -> IO [a]
parseMany p f = do
  input <- readFile f
  case runParser (many p <* eof) () f input of
    Left err -> error $ show err
    Right x  -> return x

sepP :: Parser String
sepP = string ", "

intP :: Parser Integer
intP = Tokens.integer haskell <* string ".0000000000"

floatP :: Parser Double
floatP = negP <|> Tokens.float haskell
  where negP = (\i -> -i) <$> (string "-" *> Tokens.float haskell)

-------------------------
-- File-system utilities.

cd :: FilePath -> IO a -> IO a
cd = withCurrentDirectory

listDirs :: IO [FilePath]
listDirs = sort <$> (getCurrentDirectory
                >>= listDirectory
                >>= filterM doesDirectoryExist)

listFiles :: IO [FilePath]
listFiles = sort <$> (getCurrentDirectory
                 >>= listDirectory
                 >>= filterM ((not <$>) . doesDirectoryExist))
