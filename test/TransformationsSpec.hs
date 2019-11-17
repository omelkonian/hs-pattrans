module TransformationsSpec (spec) where

import Control.Monad (forM_)

import Test.Hspec

import Types           ((.@), guessScale, createScaleInC, major)
import Transformations ((<=>), (~~), tonalTranspOf, exactOf)

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

  where
    h1 = (.@ 1) <$> [36,40,41,43,45,43,41,40]
    h2 = (.@ 1) <$> [38,41,43,45,47,45,43,41]
    h3 = (.@ 1) <$> [40,43,45,47,48,47,45,43]
    h4 = (.@ 1) <$> [41,45,47,48,50,48,47,45]
    h5 = (.@ 1) <$> [43,47,48,50,52,50,48,47]
    h6 = (.@ 1) <$> [45,48,50,52,53,52,50,48]
    h7 = (.@ 1) <$> [47,50,52,53,55,53,52,50]
    hs = [h1, h2, h3, h4, h5, h6, h7]
    
    ho = (.@ 1) <$> [36,38,40,41,43,45,43,41,40]
