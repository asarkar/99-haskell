module BinaryTree.P69Spec (spec) where

import BinaryTree.BinaryTree (fromList)
import BinaryTree.P69
import Test.Hspec

spec :: Spec
spec = do
  describe "tree2ds" $ do
    it "converts a tree to its Dotstring representation" $ do
      let tree = fromList head ["x", "y", "z", "null", "null", "0"]
      tree2ds tree `shouldBe` "xy..z0..."

  describe "ds2tree" $ do
    it "constructs a tree from its Dotstring representation" $ do
      let tree =
            fromList
              head
              [ "a",
                "b",
                "c",
                "d",
                "e",
                "null",
                "f",
                "null",
                "null",
                "null",
                "null",
                "g"
              ]

      ds2tree "abd..e..c.fg." `shouldBe` tree
