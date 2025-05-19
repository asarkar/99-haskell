module BinaryTree.P61ASpec (spec) where

import BinaryTree.P61A
import BinaryTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "leaves" $ do
    it "collects the leaves of a binary tree in a list" $ do
      leaves tree4 `shouldMatchList` [4, 2]
