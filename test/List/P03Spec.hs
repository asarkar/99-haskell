module List.P03Spec (spec) where

import List.GenList
import List.P03
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "elementAt" $ do
    prop "returns the K'th element" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, k) -> elementAt xs k `shouldBe` xs !! (k - 1)
