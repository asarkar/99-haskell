module BinaryTree.P56Spec (spec) where

import BinaryTree.BinaryTree (fromList)
import BinaryTree.P56
import Test.Hspec

spec :: Spec
spec = do
  describe "symmetric" $ do
    it "checks if a tree is symmetric" $ do
      let t1 = fromList head ["x", "x"]
      symmetric t1 `shouldBe` False

      let t2 = fromList head ["x", "x", "x"]
      symmetric t2 `shouldBe` True
