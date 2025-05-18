module Arithmetic.P36Spec (spec) where

import Arithmetic.P36
import Test.Hspec

spec :: Spec
spec = do
  describe "primeFactorsMult" $ do
    it "calculates the prime factors and their multiplicities of a positive integer" $ do
      primeFactorsMult 315 `shouldBe` [(3, 2), (5, 1), (7, 1)]
