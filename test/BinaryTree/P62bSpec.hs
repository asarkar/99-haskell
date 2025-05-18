module BinaryTree.P62bSpec (spec) where

import BinaryTree.P62b
import BinaryTree.Trees
import Test.Hspec

spec :: Spec
spec = do
  describe "atLevel" $ do
    it "collects the nodes at a given level in a list" $ do
      atLevel tree4 2 `shouldMatchList` [2, 2]
