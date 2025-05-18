module List.P29Spec (spec) where

import List.P29
import Test.Hspec

spec :: Spec
spec = do
  describe "fibonacci" $ do
    it "generates the nth Fibonacci number" $ do
      map fibonacci [1 .. 8] `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13]
