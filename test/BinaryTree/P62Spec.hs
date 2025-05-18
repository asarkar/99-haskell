module BinaryTree.P62Spec (spec) where

import BinaryTree.P62
import BinaryTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "internals" $ do
    it "collects the internal nodes of a binary tree in a list" $ do
      internals tree4 `shouldMatchList` [1, 2]
