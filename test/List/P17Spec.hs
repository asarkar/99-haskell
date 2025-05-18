module List.P17Spec (spec) where

import qualified Data.List as L
import List.GenList
import List.P17
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "split" $ do
    prop "splits a list into two parts" $ do
      forAll (genNEListAndLen genNum) $
        \(xs, n) -> do
          split xs n `shouldBe` L.splitAt n xs
