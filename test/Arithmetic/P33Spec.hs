module Arithmetic.P33Spec (spec) where

import Arithmetic.P33
import Test.Hspec

spec :: Spec
spec = do
  describe "coprime" $ do
    it "determines whether two positive integers are coprime" $ do
      coprime 35 64 `shouldBe` True
      coprime 1173 1547 `shouldBe` False
