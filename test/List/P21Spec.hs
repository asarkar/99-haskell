module List.P21Spec (spec) where

import qualified Control.Exception.Base as B
import List.GenList
import List.P21
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "insertAt" $ do
    prop "inserts an element at a given position" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, n) -> insertAt 1 xs n !! (n - 1) `shouldBe` 1

    it "throws an error when the given position is less than 1" $ do
      B.evaluate (insertAt 'X' "abcd" (-1)) `shouldThrow` anyErrorCall
