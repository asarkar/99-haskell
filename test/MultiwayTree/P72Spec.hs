module MultiwayTree.P72Spec (spec) where

import MultiwayTree.P72
import MultiwayTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "bottomUp" $ do
    it "constructs the bottom-up sequence of the nodes" $ do
      bottomUp tree5 `shouldBe` "gfcdeba"
