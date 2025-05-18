module BinaryTree.P58Spec (spec) where

import BinaryTree.BinaryTree (fromList)
import BinaryTree.P58
import Test.Hspec

spec :: Spec
spec = do
  describe "symCbalTrees" $ do
    it "constructs completely balanced symmetric binary trees" $ do
      let trees =
            map
              (fromList head)
              [ ["x", "x", "x", "null", "x", "x"],
                ["x", "x", "x", "x", "null", "null", "x"]
              ]

      symCbalTrees 5 `shouldMatchList` trees
