module TransformationsSpec (spec) where

import Types           (Note(..), (.@), guessScale, createScaleInC, major)
import Transformations ((<=>), (~~), tonalTranspOf)

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do
  let o1 = (.@ 1) <$> [36,40,41,43,45,43,41,40]
  let o2 = (.@ 1) <$> [38,41,43,45,47,45,43,41]

  describe "guessScale" $ do
    it "guesses the scale/mode of a musical sequence" $
      guessScale (o1++o2) `shouldBe` createScaleInC major
          
  describe "tonalTransposition" $ do
    it "correctly detects tonal transposition" $
      (o1 <=> o2) (tonalTranspOf ~~ 1)
