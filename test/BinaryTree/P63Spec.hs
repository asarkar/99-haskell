module BinaryTree.P63Spec (spec) where

import BinaryTree.BinaryTree (fromList)
import BinaryTree.P63
import Test.Hspec

spec :: Spec
spec = do
  describe "completeBinaryTree" $ do
    it "constructs a complete binary tree" $ do
      let t1 = fromList head ["x", "x", "x", "x"]
      completeBinaryTree (4 :: Int) `shouldBe` t1

      let t2 = fromList head ["x"]
      completeBinaryTree (1 :: Int) `shouldBe` t2

      let t3 = fromList head ["x", "x", "x", "x", "x"]
      completeBinaryTree (5 :: Int) `shouldBe` t3
