module List.P05Spec (spec) where

import List.GenList
import List.P05
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "myReverse" $ do
    prop "reverses a list" $ do
      forAll (genNEList genNum) $
        \xs -> myReverse xs `shouldBe` reverse xs
