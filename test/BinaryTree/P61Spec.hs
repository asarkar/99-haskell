module BinaryTree.P61Spec (spec) where

import BinaryTree.P61
import BinaryTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "countLeaves" $ do
    it "counts the leaves of a binary tree" $ do
      countLeaves tree4 `shouldBe` 2
