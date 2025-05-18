module Logic.P49Spec (spec) where

import Logic.P49
import Test.Hspec

spec :: Spec
spec = do
  describe "gray" $ do
    it "generates gray codes" $ do
      gray 3 `shouldBe` ["000", "001", "011", "010", "110", "111", "101", "100"]
