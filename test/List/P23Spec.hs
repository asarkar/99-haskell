module List.P23Spec (spec) where

import List.GenList
import List.P23
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "rndSelect" $ do
    prop "extracts a given number of randomly selected elements" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, n) -> do
          ys <- rndSelect xs n
          length ys `shouldBe` n
          all (`elem` xs) ys `shouldBe` True
