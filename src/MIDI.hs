module MIDI (readFromMidi, writeToMidi) where

import Euterpea.Music (Music(..), AbsPitch, Pitch, Volume, line, absPitch)
import qualified Euterpea.Music as M
import Euterpea.IO.MIDI.ToMidi (writeMidi)
import Euterpea.IO.MIDI.FromMidi2 (fromMidi2)
import qualified Codec.Midi as MIDI

import Types
import Transformations

writeToMidi :: FilePath -> Pattern -> IO ()
writeToMidi fn = writeMidi fn . patternToMusic
  where
    patternToMusic :: Pattern -> Music AbsPitch
    patternToMusic = line . fmap convert . withDurations

    withDurations :: Pattern -> [(MIDI, Time)]
    withDurations ps = zip (pitch ps) (rhythm ps ++ [4])

    convert :: (MIDI, Time) -> Music AbsPitch
    convert (m, tt) = Prim $ M.Note (toRational tt) (fromInteger m)

readFromMidi :: FilePath -> IO Pattern
readFromMidi = fmap (musicToPattern . fromMidi2) . importFile
  where
    musicToPattern :: Music (Pitch, Volume) -> Pattern
    musicToPattern = withDurations . convert . fmap (absPitch . fst)

    convert :: Music AbsPitch -> [Either Time Note]
    convert (n :+: ns)         = convert n ++ convert ns
    convert (n :=: _)          = convert n
    convert (Modify _ n)       = convert n
    convert (Prim prim) =
      case prim of
        M.Rest r   -> [Left $ fromRational r]
        M.Note d p -> [Right $ Note (fromRational d) (toInteger p)]

    withDurations :: [Either Time Note] -> [Note]
    withDurations = snd . foldl f (0.0, [])
      where f :: (Double, [Note]) -> Either Time Note -> (Double, [Note])
            f (acc, ns) (Left tt)           = (acc + tt, ns)
            f (acc, ns) (Right (Note tt m)) = (acc', ns ++ [Note acc' m])
              where acc' = acc + tt

    importFile :: FilePath -> IO MIDI.Midi
    importFile fn = do
      r <- MIDI.importFile fn
      case r of
        Left err -> error err
        Right m  -> return m




