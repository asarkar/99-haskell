module List.P10Spec (spec) where

import List.P10
import Test.Hspec

spec :: Spec
spec = do
  describe "encode" $ do
    it "creates Run-length encoding" $ do
      encode "aaaabccaadeeee"
        `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]
