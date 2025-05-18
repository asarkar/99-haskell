module BinaryTree.P55Spec (spec) where

import BinaryTree.BinaryTree (Tree (..), fromList)
import BinaryTree.P55
import Test.Hspec

spec :: Spec
spec = do
  describe "cbalTree" $ do
    it "constructs completely balanced binary trees" $ do
      let trees =
            map
              (fromList head)
              [ ["x", "x", "x", "null", "null", "x"],
                ["x", "x", "x", "x"],
                ["x", "x", "x", "null", "null", "null", "x"],
                ["x", "x", "x", "null", "x"]
              ]

      (cbalTree 4 :: [Tree Char]) `shouldMatchList` trees
