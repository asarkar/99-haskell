module Arithmetic.P32Spec (spec) where

import Arithmetic.P32
import Test.Hspec

spec :: Spec
spec = do
  describe "myGCD" $ do
    it "calculates the GCD of two positive integers" $ do
      myGCD 36 63 `shouldBe` 9
      myGCD 125 81 `shouldBe` 1
      myGCD 221 559 `shouldBe` 13
