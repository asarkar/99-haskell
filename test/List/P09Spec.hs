module List.P09Spec (spec) where

import qualified Data.List as L
import List.GenList
import List.P09
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "pack" $ do
    prop "packs consecutive duplicates of list elements into sublists" $ do
      forAll (genRepeatedElem genNum) $
        \xs -> pack xs `shouldBe` L.group xs
