module List.P01Spec (spec) where

import qualified Data.List as L
import List.GenList
import List.P01
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "myLast" $ do
    prop "returns the last element" $ do
      forAll (genNEList genNum) $
        \xs -> myLast xs `shouldBe` L.last xs
