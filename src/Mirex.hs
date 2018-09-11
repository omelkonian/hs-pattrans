module Mirex ( MirexDataset
             , MirexEntry(..)
             , decode
             ) where

import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token    as Tokens

type MirexDataset = [MirexEntry]
data MirexEntry = MirexEntry { ontime   :: Double
                             , midi     :: Integer
                             , morph    :: Integer
                             , duration :: Double
                             , staff    :: Integer
                             }
                             deriving (Eq, Show)

-- | Run parser on the MIREX dataset.
decode :: FilePath -> IO MirexDataset
decode fname = do
  input <- readFile fname
  case runParser (many mirexP <* eof) () fname input of
    Left err -> error $ show err
    Right x  -> return x

-- | Parse a line of the MIREX dataset.
mirexP :: Parser MirexEntry
mirexP = MirexEntry <$> (floatP <* sepP) <*> (intP   <* sepP)
                    <*> (intP   <* sepP) <*> (floatP <* sepP)
                    <*> intP <* newline
  where
    sepP = string ", "
    intP = Tokens.integer haskell <* string ".0000000000"
    floatP = Tokens.float haskell
