module TransformationsSpec (spec) where

import Control.Monad (forM_)

import Test.Hspec

import Types
import Transformations ((<=>), (~~), tonalTranspOf, exactOf, retrogradeOf, inversionOf, transpositionOf, rotationOf, augmentationOf, trInversionOf, trAugmentationOf, tonalTranspOfCan, tonalInversionOfCan)

forAll :: Example r => [a] -> String -> (a -> r) -> SpecWith (Arg r)
forAll xs title k = 
  forM_ exs $ \(i, x) -> do
    describe title $ do
      it ("# " ++ show i) $
        k x
  where exs = zip [1..] xs

forAll2 :: Example r => [a] -> String -> (a -> a -> r) -> SpecWith (Arg r)
forAll2 xs title k = 
  forM_ [(x, y) | x <- exs, y <- exs, fst x < fst y] $ \((i, x), (i', x')) -> do
    describe title $ do
      it (show i ++ " ~ " ++ show i') $
        k x x'
  where exs = zip [1..] xs

createPitchesfromInterval :: MIDI -> [Interval] -> Length -> [MIDI]
createPitchesfromInterval m i l = map (+m) (take l (scanl1 (+) (cycle i)))

createPitchesfromScaleDegree :: Length -> MIDI -> ScaleType -> [ScaleDegree] -> [MIDI]
createPitchesfromScaleDegree l n st degrees =  take l seqMultipleOctaves
  where
    seqMultipleOctaves :: [MIDI]
    seqMultipleOctaves = [(+) (o * 12) | o <- [0 .. 7]] <*> seqOneOctave 
    
    seqOneOctave :: [MIDI]
    seqOneOctave = oneOctaveLookup degrees oneOctaveScale

    oneOctaveLookup :: [ScaleDegree] -> [(MIDI, (ScaleDegree, Octave))] -> [MIDI]
    oneOctaveLookup [] _ = []
    oneOctaveLookup (sd:sds) scale = map fst (filter (\x -> (fst $ snd x) == sd) scale) ++ oneOctaveLookup sds scale

    oneOctaveScale = [ (n + m, (i, 1)) | (i, m) <- zip [1..7] st]


spec :: Spec
spec = do
  
  forAll2 hs "guessScale" $ \x y -> do
    guessScale (x++y) `shouldBe` createScaleInC major

  forAll2 hs "tonal transposition - hanons" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)

  forAll2 tris "tonal transposition - triads" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 neighBb "tonal transposition - neighbour notes in Bb" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 neighDu5u "This one will fail: use tonalTranspCan (the one after next test) instead. tonal transposition - neighbour notes in D up a fifth and neighbouring up" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 neighDu5u "guess scale - neighbour notes in D up a fifth and neighbouring up" $ \x y -> do
    guessScale (x++y) `shouldBe` createScaleInD major
  
  forAll2 neighDu5u "tonal transposition more candidates - neighbour notes in D up a fifth and neighbouring up" $ \x y -> do
      (x <=> y) (tonalTranspOfCan ~~ 1)
      
  forAll2 neighDu5d "This one will fail: use tonalTranspCan (next test) instead. tonal transposition - neighbour notes in D up a fifth and neighbouring down" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 neighDu5d "tonal transposition more candidates - neighbour notes in D up a fifth and neighbouring down" $ \x y -> do
      (x <=> y) (tonalTranspOfCan ~~ 1)
  
  forAll2 neighAd5d "tonal transposition - neighbour notes in A down a fifth and neighbouring down" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 reachF "tonal transposition - reaching notes in F" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 escapeG "tonal transposition - escaping notes in G" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 launchingEb "tonal transposition - launching notes in Eb" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  forAll2 landingAm "tonal transposition - landing notes in A minor" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)
  
  describe "retrograde" $ do
    it "correctly detects self-retrograde with palindromeven" $
      (palindromeeven <=> palindromeeven) (retrogradeOf ~~ 1)
      
  describe "retrograde" $ do
    it "correctly detects self-retrograde with palindromeodd" $
      (palindromeodd <=> palindromeodd) (retrogradeOf ~~ 1)

  describe "real transposition" $ do
    it "correctly detects real transposition" $
      (h1 <=> h1trans) (transpositionOf ~~ 1)

  describe "exact approx" $ do
    it "correctly detects exact repetition with approximation - deleted first note" $
      (h1trans <=> h1transdel1) (exactOf ~~ 0.8)
  
  describe "exact approx" $ do
    it "correctly detects exact repetition with approximation - deleted second note" $
      (h1trans <=> h1transdel2) (exactOf ~~ 0.8)

  describe "exact approx" $ do
    it "correctly detects exact repetition with approximation - deleted third note" $
      (h1trans <=> h1transdel3) (exactOf ~~ 0.8)
  
  describe "exact approx" $ do
    it "correctly detects exact repetition with approximation - deleted last note" $
      (h1trans <=> h1transdel4) (exactOf ~~ 0.8)
  
  describe "exact approx" $ do
    it "correctly detects exact repetition with approximation - inserted first note" $
      (h1trans <=> h1transins1) (exactOf ~~ 0.8)
  
  describe "exact approx" $ do
    it "correctly detects exact repetition with approximation - inserted first three note" $
      (h1trans <=> h1transins2) (exactOf ~~ 0.7)

  describe "exact approx" $ do
    it "correctly detects exact repetition with approximation - inserted last note" $
      (h1trans <=> h1transins3) (exactOf ~~ 0.8)

  describe "transposition approx" $ do
    it "correctly detects real transformation with approximation - deleted first note" $
      (h1 <=> h1transdel1) (transpositionOf ~~ 0.8)
  
  describe "transposition approx" $ do
    it "correctly detects real transformation with approximation - deleted second note, middle notes are different from first and last note (changing two intervals ~~ 0.71)" $
      (h1 <=> h1transdel2) (transpositionOf ~~ 0.71)

  describe "transposition approx" $ do
    it "correctly detects real transformation with approximation - deleted third note (changing two intervals ~~ 0.71)" $
      (h1 <=> h1transdel3) (transpositionOf ~~ 0.71)
  
  describe "transposition approx" $ do
    it "correctly detects real transformation with approximation - deleted last note" $
      (h1 <=> h1transdel4) (transpositionOf ~~ 0.8)
  
  describe "transposition approx" $ do
    it "correctly detects real transformation with approximation - inserted first note" $
      (h1 <=> h1transins1) (transpositionOf ~~ 0.8)
  
  describe "transposition approx" $ do
    it "correctly detects real transformation with approximation - inserted first three note" $
      (h1 <=> h1transins2) (transpositionOf ~~ 0.7)

  describe "transposition approx" $ do
    it "correctly detects real transformation with approximation - inserted last note" $
      (h1 <=> h1transins3) (transpositionOf ~~ 0.8)
  
  describe "tonal inversion" $ do
    it "correctly detects tonal inversion with hanon C" $
      (h1 <=> hback1) (tonalInversionOfCan ~~ 1)

  describe "tonal inversion" $ do
    it "correctly detects tonal inversion with hanon D" $
      (h2 <=> hback2) (tonalInversionOfCan ~~ 1)
  
  describe "inversion" $ do
    it "correctly detects real inversion with the triplet" $
      (triplet <=> tripletrealinv) (inversionOf ~~ 1)
      
  describe "transposed inversion" $ do
    it "correctly detects real transposed inversion with the triplet" $
      (triplet <=> tripletrealinvtrans) (trInversionOf ~~ 1)
  
  describe "rotation" $ do
    it "correctly detects rotation with the triplet" $
      (triplet <=> tripletRI) (rotationOf ~~ 1)

  describe "augmentation" $ do
    it "correctly detects augmentation with the explicit notes" $
      (cna <=> ca) (augmentationOf ~~ 1)

  describe "augmentation" $ do
    it "correctly detects trans augmentation with the explicit notes" $
      (cat <=> ca) (trAugmentationOf ~~ 1)
 
  
  where
    h = createPitchesfromScaleDegree 8 36 major [1,3,4,5,6,5,4,3]
    h1 = (.@@) [1..] h
    h2 = (.@@) [1..] [38,41,43,45,47,45,43,41]
    h3 = (.@@) [1..] [40,43,45,47,48,47,45,43]
    h4 = (.@@) [1..] [41,45,47,48,50,48,47,45]
    h5 = (.@@) [1..] [43,47,48,50,52,50,48,47]
    h6 = (.@@) [1..] [45,48,50,52,53,52,50,48]
    h7 = (.@@) [1..] [47,50,52,53,55,53,52,50]
    hs = [h1, h2, h3, h4, h5, h6, h7]

    triC = (.@@) [1..] [36,40,43]
    triD = (.@@) [1..] [38,41,45]
    triE = (.@@) [1..] [40,43,47]
    triF = (.@@) [1..] [41,45,48]
    triG = (.@@) [1..] [43,47,50]
    tris = [triC, triD, triE, triF, triG]

    neighBb1 = (.@@) [1..] [34,36,34]
    neighBb2 = (.@@) [1..] [36,38,36]
    neighBb3 = (.@@) [1..] [38,39,38]
    neighBb4 = (.@@) [1..] [39,41,39]
    neighBb5 = (.@@) [1..] [41,43,41]
    neighBb = [neighBb1, neighBb2, neighBb3, neighBb4, neighBb5]
    
    neighD1u5d = (.@@) [1..] [38,45,53,45]
    neighD2u5d = (.@@) [1..] [40,47,45,47]
    neighD3u5d = (.@@) [1..] [42,49,47,49]
    neighD4u5d = (.@@) [1..] [43,50,49,50]
    neighDu5d = [neighD1u5d, neighD2u5d, neighD3u5d, neighD4u5d]
  
    neighD1u5u = (.@@) [1..] [38,45,57,45]
    neighD2u5u = (.@@) [1..] [40,47,49,47]
    neighD3u5u = (.@@) [1..] [42,49,50,49]
    neighD4u5u = (.@@) [1..] [43,50,52,50]
    neighDu5u = [neighD1u5u, neighD2u5u, neighD3u5u, neighD4u5u]
  
    neighA1d5d = (.@@) [1..] [45,38,37,38]
    neighA2d5d = (.@@) [1..] [47,40,38,40]
    neighA3d5d = (.@@) [1..] [49,42,40,42]
    neighA4d5d = (.@@) [1..] [50,44,42,44]
    neighAd5d = [neighA1d5d, neighA2d5d, neighA3d5d, neighA4d5d]
  
    reachF1 = (.@@) [1..] [41,46,45]
    reachF2 = (.@@) [1..] [43,48,46]
    reachF3 = (.@@) [1..] [45,50,48]
    reachF = [reachF1, reachF2, reachF3]

    escapeG1 = (.@@) [1..] [43,42,47]
    escapeG2 = (.@@) [1..] [45,43,48]
    escapeG3 = (.@@) [1..] [47,45,50]
    escapeG = [escapeG1, escapeG2, escapeG3]

    launchingEb1 = (.@@) [1..] [39,41,46]
    launchingEb2 = (.@@) [1..] [41,43,48]
    launchingEb3 = (.@@) [1..] [43,44,50]
    launchingEb = [launchingEb1, launchingEb2, launchingEb3]

    landingAm1 = (.@@) [1..] [33,38,40]
    landingAm2 = (.@@) [1..] [35,40,41]
    landingAm3 = (.@@) [1..] [36,41,43]
    landingAm = [landingAm1, landingAm2, landingAm3]
  
    hback1 = (.@@) [1..] [60,57,55,53,52,53,55,57]
    hback2 = (.@@) [1..] [59,55,53,52,50,52,53,55]

    palindromeeven = (.@@) [1..] [60,62,64,64,62,60]
    palindromeodd = (.@@) [1..] [60,62,64,62,60]

    h1trans= (.@@) [1..] [37,41,42,44,46,44,42,41]
    h1transdel1 = (.@@) [1..] [41,42,44,46,44,42,41]
    h1transdel2 = (.@@) [1..] [37,42,44,46,44,42,41]
    h1transdel3 = (.@@) [1..] [37,41,42,46,44,42,41]
    h1transdel4 = (.@@) [1..] [37,41,42,44,46,44,42]
    h1transins1 = (.@@) [1..] [36,37,41,42,44,46,44,42,41]
    h1transins2 = (.@@) [1..] [34,35,36,37,41,42,44,46,44,42,41]
    h1transins3 = (.@@) [1..] [37,41,42,44,46,44,42,41,40]

    triplet = (.@@) [1..] [60,62,64]
    tripletrealinv = (.@@) [1..] [60,58,56]
    tripletrealinvtrans = (.@@) [1..] [59,57,55]
    tripletRI = (.@@) [1..] [56,58,60]
    
    cna = (.@@) [1,2,3] [36,40,41]
    ca = (.@@) [1,3..] [36,40,41]
    cat = (.@@) [1,3..] [38,42,43]
  
     
