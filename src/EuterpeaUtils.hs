module EuterpeaUtils
  ( -- re-exporting Euterpea things
    module Export
    -- conversion from/to Euterpea datatype
  , patternToMusic, musicToPattern
  ) where

import           Euterpea.Music as Export hiding (Rest, Note, pitch)
import qualified Euterpea.Music as M

import Types
import Transformations

-- | Convert our Pattern datatype to Euterpea's music datatype.
patternToMusic :: Pattern -> Music AbsPitch
patternToMusic = line . fmap convert . withDurations
  where
    withDurations :: Pattern -> [(MIDI, Time)]
    withDurations ps = zip (pitch ps) (rhythm ps ++ [4])

    convert :: (MIDI, Time) -> Music AbsPitch
    convert (m, tt) = Prim $ M.Note (toRational tt) (fromInteger m)

-- | Convert from Euterpea's music datatype to our pattern datatype.
musicToPattern :: ToMusic1 a => Music a -> Pattern
musicToPattern = withDurations . convert . fmap (absPitch . fst) . toMusic1
  where
    convert :: Music AbsPitch -> [Either Time Note]
    convert (n :+: ns)         = convert n ++ convert ns
    convert (n :=: _)          = convert n
    convert (Modify _ n)       = convert n
    convert (Prim prim) =
      case prim of
        M.Rest r   -> [Left $ fromRational r]
        M.Note dr p -> [Right $ Note (fromRational dr) (toInteger p)]

    withDurations :: [Either Time Note] -> [Note]
    withDurations = snd . foldl go (0.0, [])
      where go :: (Double, [Note]) -> Either Time Note -> (Double, [Note])
            go (acc, ns) (Left tt)           = (acc + tt, ns)
            go (acc, ns) (Right (Note tt m)) = (acc', ns ++ [Note acc' m])
              where acc' = acc + tt
