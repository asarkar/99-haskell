module MultiwayTree.P70Spec (spec) where

import MultiwayTree.P70
import MultiwayTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "stringToTree" $ do
    it "constructs a multiway tree from a string" $ do
      let tree = stringToTree "afg^^c^bd^e^^^"
      tree `shouldBe` t1

  describe "treeToString" $ do
    it "converts a multiway tree to a string" $ do
      treeToString t1 `shouldBe` "afg^^c^bd^e^^^"
