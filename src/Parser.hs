module Parser ( Time
              , MIDI
              , MirexEntry(..)
              , PatternType(..)
              , Pattern
              , Note(..), (~>), (<~)
              , ParseOption(..)
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

-- | MIREX datatypes.
type Time = Double
type MIDI = Integer

data MirexEntry = MirexEntry { t        :: Time
                             , mid      :: MIDI
                             , morph    :: Integer
                             , duration :: Time
                             , staff    :: Integer
                             }
                             deriving (Eq, Show)

data PatternType = PatternType { piece_name   :: String
                               , expert_name  :: String
                               , pattern_name :: String
                               , basePattern  :: Pattern
                               , patterns     :: [Pattern]
                               } deriving (Eq, Show)

newtype Pattern = Pattern [Note] deriving (Show)

data Note = Note { ontime :: Time
                 , midi   :: MIDI
                 } deriving (Eq, Show)

-- | Transpose a note in time.
(<~), (~>) :: Note -> Time -> Note
Note tInit m <~ dt = Note (tInit - dt) m
Note tInit m ~> dt = Note (tInit + dt) m

-- | Two patterns are 'exactly' equal, when they are copies transposed in time.
instance Eq Pattern where
  Pattern ns == Pattern ns' = normalize ns == normalize ns'
    where
      normalize :: [Note] -> [Note]
      normalize (Note dt m : xs) = Note 0 m : ((<~ dt) <$> xs)
      normalize []               = []

-- | Parser options.
data ParseOption = Monophonic | Polyphonic
instance Show ParseOption where
  show Monophonic = "monophonic"
  show Polyphonic = "polyphonic"

-- | Get a specific piece of music and all its patterns from the MIREX dataset.
parseMirex :: FilePath -> ParseOption -> IO ([MirexEntry], [PatternType])
parseMirex f_root pOpt = cd (f_root ++ "/" ++ show pOpt) $ do
  -- Parse music
  music <- cd "csv" $ do
    f_music:[] <- listFiles
    parseMany mirexP f_music

  -- Parse patterns
  patTypes <- cd "repeatedPatterns" $ do
    f_patExs <- listDirs
    allPats <- forM f_patExs $ \f_patEx -> cd f_patEx $ do
      f_patTys <- listDirs
      forM f_patTys $ \f_patTy -> do
        basePat:pats <- cd (f_patTy ++ "/occurrences/csv") $ do
          f_pats <- listFiles
          forM f_pats ((Pattern <$>) . parseMany noteP)
        return $ PatternType { piece_name   = f_root
                             , expert_name  = f_patEx
                             , pattern_name = f_patTy
                             , basePattern  = basePat
                             , patterns     = pats
                             }
    return $ concat allPats
  return (music, patTypes)

-- | Parse one entry from a MIREX piece of music.
mirexP :: Parser MirexEntry
mirexP = MirexEntry <$> (floatP <* sepP) <*> (intP   <* sepP)
                    <*> (intP   <* sepP) <*> (floatP <* sepP)
                    <*> intP <* newline


-- | Parse one entry from a MIREX pattern.
noteP :: Parser Note
noteP = Note <$> (floatP <* sepP)
             <*> intP <* newline

-- | Parser utilities.
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

-- | File-system utilities.
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
