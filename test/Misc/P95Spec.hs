module Misc.P95Spec (spec) where

import Misc.P95
import Test.Hspec

spec :: Spec
spec = do
  describe "fullWords" $ do
    it "converts integers to full words" $ do
      fullWords 175 `shouldBe` "one-seven-five"
      fullWords 0 `shouldBe` "zero"
      fullWords 1 `shouldBe` "one"
