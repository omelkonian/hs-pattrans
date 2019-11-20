module TransformationsSpec (spec) where

import Control.Monad (forM_)

import Test.Hspec

import Types           ((.@), guessScale, createScaleInC, major)
import Transformations ((<=>), (~~), tonalTranspOf, exactOf, retrogradeOf, inversionOf, transpositionOf, rotationOf, augmentationOf, trInversionOf, trAugmentationOf)

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

spec :: Spec
spec = do
  
  forAll2 hs "guessScale" $ \x y -> do
    guessScale (x++y) `shouldBe` createScaleInC major

  forAll2 hs "tonal transposition" $ \x y -> do
      (x <=> y) (tonalTranspOf ~~ 1)

  describe "Approximation" $ do
    it "correctly detects Approximation 0.8 - Hanon bar 1 with one more note" $
      (h1 <=> ho) (exactOf ~~ 0.8)

  describe "exact o2-o3 in SYMCHM" $ do
    it "correctly compare pattern2 o2 and o3 in Daar ging een heer using SYMCHM" $
      (o2 <=> o3) (exactOf ~~ 0.8)

  describe "exact o1-o2 in SYMCHM" $ do
    it "correctly compare pattern2 o1 and o2 in Daar ging een heer using SYMCHM" $
      (o1 <=> o2) (exactOf ~~ 0.8)

  describe "exact o1-o3 in SYMCHM" $ do
    it "correctly compare pattern2 o1 and o3 in Daar ging een heer using SYMCHM" $
      (o1 <=> o3) (exactOf ~~ 0.8)
  
  describe "retrograde" $ do
    it "correctly detects self-retrograde with palindromeven" $
      (palindromeeven <=> palindromeeven) (retrogradeOf ~~ 1)
      
  describe "retrograde" $ do
    it "correctly detects self-retrograde with palindromeodd" $
      (palindromeodd <=> palindromeodd) (retrogradeOf ~~ 1)

  describe "real transposition" $ do
    it "correctly detects real transposition" $
      (h1 <=> h1trans) (transpositionOf ~~ 1)

  describe "real transposition with approx" $ do
    it "correctly detects real transposition with approximation - deleted first note" $
      (h1 <=> h1transdel1) (transpositionOf ~~ 0.8)
  
  describe "real transposition with approx" $ do
    it "correctly detects real transposition with approximation - deleted second note" $
      (h1 <=> h1transdel2) (transpositionOf ~~ 0.8)

  describe "tonal inversion" $ do
    it "correctly detects tonal inversion with hanon C" $
      (h1 <=> hback1) (inversionOf ~~ 0.2)

  describe "tonal inversion" $ do
    it "correctly detects tonal inversion with hanon D" $
      (h2 <=> hback2) (inversionOf ~~ 0.2)
  
  describe "inversion" $ do
    it "correctly detects real inversion with the triplet" $
      (triplet <=> tripletrealinv) (inversionOf ~~ 1)
      
  describe "transposed inversion" $ do
    it "correctly detects real transposed inversion with the triplet" $
      (triplet <=> tripletrealinvtrans) (inversionOf ~~ 1)
  
  describe "rotation" $ do
    it "correctly detects rotation with the triplet" $
      (triplet <=> tripletRI) (rotationOf ~~ 1)

  describe "augmentation" $ do
    it "correctly detects augmentation with the triplet" $
      (triplet <=> tripletaug) (augmentationOf ~~ 1)

  describe "trans augmentation" $ do
    it "correctly detects trans augmentation with the triplet" $
      (triplet <=> tripletaugtrans) (augmentationOf ~~ 1)
  
  where
    h1 = (.@ 1) <$> [36,40,41,43,45,43,41,40]
    h2 = (.@ 1) <$> [38,41,43,45,47,45,43,41]
    h3 = (.@ 1) <$> [40,43,45,47,48,47,45,43]
    h4 = (.@ 1) <$> [41,45,47,48,50,48,47,45]
    h5 = (.@ 1) <$> [43,47,48,50,52,50,48,47]
    h6 = (.@ 1) <$> [45,48,50,52,53,52,50,48]
    h7 = (.@ 1) <$> [47,50,52,53,55,53,52,50]
    hs = [h1, h2, h3, h4, h5, h6, h7]

    hback1 = (.@ 1) <$> [60,57,55,53,52,53,55,57]
    hback2 = (.@ 1) <$> [59,55,53,52,50,52,53,55]

    palindromeeven = (.@ 1) <$> [60,62,64,64,62,60]
    palindromeodd = (.@ 1) <$> [60,62,64,62,60]

    h1trans= (.@ 1) <$> [37,41,42,44,46,44,42,41]
    h1transdel1 = (.@ 1) <$> [41,42,44,46,44,42,41]
    h1transdel2 = (.@ 1) <$> [37,42,44,46,44,42,41]

    triplet = (.@ 1) <$> [60,62,64]
    tripletrealinv = (.@ 1) <$> [60,58,56]
    tripletrealinvtrans = (.@ 1) <$> [59,57,55]
    tripletaug = (.@ 2) <$> [60,62,64]
    tripletaugtrans = (.@ 2) <$> [62,64,66]
    tripletRI = (.@ 1) <$> [56,58,60]
    
    ho = (.@ 1) <$> [36,38,40,41,43,45,43,41,40]

    o1 = (.@ 1) <$> [69
        , 67
        , 62
        , 67
        , 69
        , 71
        , 71
        , 71
        , 71
        , 71
        , 69
        , 67
        , 62
        , 67
        , 69
        , 71
        , 71
        , 71
        , 71
        , 71
        , 69
        , 67
        , 71
        , 74
        , 74
        , 72
        , 71
        , 71
        , 69]
  
    o2 = (.@ 1) <$> [ 69
          , 67
          , 62
          , 67
          , 69
          , 71
          , 71
          , 71
          , 71
          , 71
          , 69
          , 67
          , 62
          , 67
          , 69
          , 71
          , 71
          , 71
          , 71
          , 71
          , 69
          , 67
          , 71
          , 74
          , 74
          , 72
          , 71
          , 71
          , 69]
  
    o3 = (.@ 1) <$> [ 69
          , 67
          , 62
          , 67
          , 69
          , 71
          , 71
          , 71
          , 71
          , 71
          , 69
          , 67
          , 62
          , 67
          , 69
          , 71
          , 71
          , 71
          , 71
          , 71
          , 69
          , 67
          , 71
          , 74
          , 72
          , 71
          , 71
          , 69]
  
