module Arithmetic.P40Spec (spec) where

import Arithmetic.P40
import Test.Hspec

spec :: Spec
spec = do
  describe "goldbach" $ do
    it "finds two prime numbers that sum to a given even integer" $ do
      goldbach 28 `shouldBe` (5, 23)
