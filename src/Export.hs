module Export (writeToMidi) where

import qualified Euterpea.Music as M
import Euterpea.IO.MIDI.ToMidi (writeMidi)

import Types
import Transformations

writeToMidi :: FilePath -> Pattern -> IO ()
writeToMidi fn = writeMidi fn . patternToMusic
  where
    patternToMusic :: Pattern -> M.Music M.AbsPitch
    patternToMusic = M.line . fmap convert . withDurations

    withDurations :: Pattern -> [(MIDI, Time)]
    withDurations ps = zip (pitch ps) (rhythm ps ++ [4])

    convert :: (MIDI, Time) -> M.Music M.AbsPitch
    convert (m, tt) = M.Prim $ M.Note (toRational tt) (fromInteger m)
