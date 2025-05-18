module Arithmetic.P39Spec (spec) where

import Arithmetic.P39
import Test.Hspec

spec :: Spec
spec = do
  describe "primesR" $ do
    it "constructs a list of primes within a given range" $ do
      primesR 10 20 `shouldBe` [11, 13, 17, 19]
      primesR 7 31 `shouldBe` [7, 11, 13, 17, 19, 23, 29, 31]
