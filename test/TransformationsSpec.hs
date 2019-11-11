module TransformationsSpec (spec) where

import Types           (Note(..), (.@), guessScale, createScaleInC, major)
import Transformations ((<=>), (~~), tonalTranspOf)

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do
  let h1 = (.@ 1) <$> [36,40,41,43,45,43,41,40]
  let h2 = (.@ 1) <$> [38,41,43,45,47,45,43,41]
  let h3 = (.@ 1) <$> [40,43,45,47,48,47,45,43]
  let h4 = (.@ 1) <$> [41,45,47,48,50,48,47,45]
  let h5 = (.@ 1) <$> [43,47,48,50,52,50,48,47]
  let h6 = (.@ 1) <$> [45,48,50,52,53,52,50,48]
  let h7 = (.@ 1) <$> [47,50,52,53,55,53,52,50]

  describe "guessScale" $ do
    it "guesses the scale/mode of a musical sequence - concatenated Hanon" $
      guessScale (h1++h2) `shouldBe` createScaleInC major
          
  describe "guessScale" $ do
    it "guesses the scale/mode of a musical sequence - concatenated Hanon" $
      guessScale (h3++h4) `shouldBe` createScaleInC major

  describe "guessScale" $ do
    it "guesses the scale/mode of a musical sequence - concatenated Hanon" $
      guessScale (h5++h6) `shouldBe` createScaleInC major

  describe "tonalTransposition" $ do
    it "correctly detects tonal transposition - Hanon bar 1-2" $
      (h1 <=> h2) (tonalTranspOf ~~ 1)

  describe "tonalTransposition" $ do
    it "correctly detects tonal transposition - Hanon bar 2-3" $
      (h2 <=> h3) (tonalTranspOf ~~ 1)
  
  describe "tonalTransposition" $ do
    it "correctly detects tonal transposition - Hanon bar 3-4" $
      (h3 <=> h4) (tonalTranspOf ~~ 1)
      
  describe "tonalTransposition" $ do
    it "correctly detects tonal transposition - Hanon bar 4-5" $
      (h4 <=> h6) (tonalTranspOf ~~ 1)
