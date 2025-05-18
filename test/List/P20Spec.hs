module List.P20Spec (spec) where

import List.P20
import Test.Hspec

spec :: Spec
spec = do
  describe "removeAt" $ do
    it "removes the K'th element" $ do
      removeAt 2 "abcd" `shouldBe` ('b', "acd")
