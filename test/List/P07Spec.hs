module List.P07Spec (spec) where

import List.P07
import Test.Hspec

spec :: Spec
spec = do
  describe "flatten" $ do
    it "flattens a nested list structure" $ do
      flatten (Elem (5 :: Int)) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem (5 :: Int)]])
        `shouldBe` [1 .. 5]
      flatten (List []) `shouldBe` ([] :: [Int])
