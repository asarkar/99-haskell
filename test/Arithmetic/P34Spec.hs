module Arithmetic.P34Spec (spec) where

import Arithmetic.P34
import Test.Hspec

spec :: Spec
spec = do
  describe "totient" $ do
    it "calculates Euler's totient function phi" $ do
      totient 10 `shouldBe` 4
