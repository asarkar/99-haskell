module List.P04Spec (spec) where

import List.GenList
import List.P04
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "myLength" $ do
    prop "finds the number of elements" $ do
      forAll (genNEList genNum) $
        \xs -> myLength xs `shouldBe` length xs
