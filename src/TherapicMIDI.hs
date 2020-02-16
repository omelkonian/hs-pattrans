module TherapicMIDI where

-- improved based on https://github.com/Kumodatsu/Therapic
import qualified Codec.Midi as MIDI
import Data.List                  (sortOn)
import Data.Maybe                 (isJust, fromJust)
import Types

----------------------------
-- * Types
----------------------------

data NoteT = NoteT {
    -- | The key or position of the played note on a chart.
    getPitch       :: Int,
    -- | The start time for when this note begins in a song.
    getStartTime :: StartTime,
    -- | The length that this note will play.
    getDuration  :: Duration
  } deriving (Show)

type StartTime    = Ticks
type EndTime      = Ticks
type Duration     = Ticks
type Ticks        = Int

data MID = MID {
    getTrackNum     :: Int,
    getMIDINotes    :: [NoteT],
    getTickDuration :: Float
  } deriving (Show)

silentKey :: Int
silentKey = -1


getEndTime :: NoteT -> EndTime
getEndTime note = getStartTime note + getDuration note

emptyMidi :: MIDI.Midi
emptyMidi = MIDI.Midi MIDI.SingleTrack (MIDI.TicksPerBeat 0) []



----------------------------
-- * Import
----------------------------

-- | Convert a single file at the specified location to a MIDI structure.
parseMID :: Int -> FilePath -> IO Pattern
parseMID v file = do
  posMidi <- MIDI.importFile file              -- :: IO (Either String Midi)
  let mid = fromRight emptyMidi posMidi  -- Midi
  let mID =  convertMidi v mid
  return $ convertMidNote mID

convertMidNote :: MID -> [Note]
convertMidNote (MID _ ns ticks) = pat
  where
    pat = map convert ns
    convert :: NoteT -> Note
    convert (NoteT pit start _) = Note (realToFrac ((fromIntegral start) * (2 * ticks))) (fromIntegral pit)

convertMidi :: Int -> MIDI.Midi -> MID
convertMidi voice (MIDI.Midi _ time tracks) = MID voice pChannel duration
  where
    pChannel = channels !! voice
    channels = map convertTrack tracks
    duration = convertTime time

convertTrack :: MIDI.Track MIDI.Ticks -> [NoteT]
convertTrack = finaliseSweep . foldl evaluateSweep initialStatus . MIDI.toAbsTime
  where
    initialStatus = ([], [])

----------------------------
-- * Evaluation functions
----------------------------
-- | Provides all the notes which have ended in the song.
-- The notes will be ordered on start time and duration/end time.
-- This will ignore notes without Off Messages in case of incorrect
-- serialisation to the Midi file.
finaliseSweep :: TrackResult -> [NoteT]
finaliseSweep (_, cs) = sortOn getStartTime $ sortOn getEndTime cs

-- | Performs one message evaluation.
-- The goal of this step is to process note information to complete all notes.
evaluateSweep :: TrackResult -> (Ticks, MIDI.Message) -> TrackResult
evaluateSweep result (t, m) | MIDI.isNoteOn   m = addNewNote   result (t, m)
                            | MIDI.isNoteOff  m = completeNote result (t, m)
                            | MIDI.isTrackEnd m = completeAll  result (t, m)
                            | otherwise    = result


----------------------------
-- * Convert Function
----------------------------
-- | Given a TimeDiv, calculates the duration of a single tick.
convertTime :: MIDI.TimeDiv -> Float
convertTime (MIDI.TicksPerSecond fps tpf) = 1.0 / fromIntegral (fps * tpf)
convertTime (MIDI.TicksPerBeat   tpb    ) = 1.0 / fromIntegral (bps * tpb)
  where
    bps = 120 `div` 60 -- Codec.Midi assumes a bpm of 120


-- | Adds a new partial note as specified by the message.
addNewNote :: TrackResult -> (Ticks, MIDI.Message) -> TrackResult
addNewNote (ps, cs) (ts, MIDI.NoteOn _ key _)
               = let partial = StartNote key ts in (partial:ps, cs)
addNewNote _ _ = error "Invalid Message type used, Expected NoteOn."

-- | Completes a previous partial note as specified by the message.
completeNote :: TrackResult -> (Ticks, MIDI.Message) -> TrackResult
completeNote (ps, cs) (ts, MIDI.NoteOff _ key _) = (incs, comps)
  where
    comps | isJust mNote = finishNote ts (fromJust mNote) : cs
          | otherwise    = cs
    (mNote, incs) = findNote key ps
completeNote _ _ = error "Invalid Message type used, Expected NoteOff."

-- | Completes all previous partial notes as the song has ended.
completeAll :: TrackResult -> (Ticks, MIDI.Message) -> TrackResult
completeAll (ps, cs) (ts, _) = ([], comps++cs)
  where
    comps = finishNotes ts ps

----------------------------
-- * Message Checks
----------------------------

-- | Checks if a given note which needs to be completely parsed has the
-- following key.
hasKey :: Int -> ParseNote -> Bool
hasKey altKey (StartNote key _) = key == altKey


-- | Adds the extra information to the partial note to make it a full note.
finishNote :: EndTime -> ParseNote -> CompleteNote
finishNote end (StartNote key start) = NoteT key start (end - start)

-- | Adds the extra information to the partial notes to make it a full notes.
finishNotes :: EndTime -> [ParseNote] -> [CompleteNote]
finishNotes t = map (finishNote t)


findNote :: Int -> [ParseNote] -> (Maybe ParseNote, [ParseNote])
findNote key ps = findNote' ps []
  where
    findNote' []     rs = (Nothing, reverse rs)
    findNote' (p:ps) rs | hasKey key p = (Just p, reverse rs ++ ps)
                        | otherwise    = findNote' ps (p:rs)

----------------------------
-- * Status Object
----------------------------

-- | Describes a single note which was started in the midi file
-- and is still playing. More information is required to make this
-- a CompleteNote.
data ParseNote = StartNote Int Int
-- | Describes a note with all the information collected from the Midi file.
type CompleteNote = NoteT

-- | Describes a collection of notes which still need their ending
-- in the song to be completed.
type TrackStatus = [ParseNote]
-- | A tuple of the playing notes and finished notes.
type TrackResult = (TrackStatus, [CompleteNote])

----------------------------
-- * Either Operations
----------------------------

-- Most of these functions are here to ensure that this program works
-- on base version 4.10 and below.

-- | Provides the left value from an either statement if it exists.
posLeft :: Either a b -> Maybe a
posLeft (Left a)  = Just a
posLeft (Right _) = Nothing

-- | Provides the right value from an either statement if it exists.
posRight :: Either a b -> Maybe b
posRight (Left  _) = Nothing
posRight (Right b) = Just b

-- | If the Either value is a Left value, returns the contained value, and
-- otherwise returns a default value.
fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x _        = x

-- | If the Either value is a Right value, returns the contained value, and
-- otherwise returns a default value.
fromRight :: b -> Either a b -> b
fromRight _ (Right x) = x
fromRight x _         = x
