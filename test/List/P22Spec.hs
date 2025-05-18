module List.P22Spec (spec) where

import List.P22
import Test.Hspec

spec :: Spec
spec = do
  describe "range" $ do
    it "creates an increasing range" $ do
      range 1 5 `shouldBe` [1, 2, 3, 4, 5]

    it "creates a decreasing range" $ do
      range 5 1 `shouldBe` [5, 4, 3, 2, 1]

    it "creates a singleton range" $ do
      range 1 1 `shouldBe` [1]
