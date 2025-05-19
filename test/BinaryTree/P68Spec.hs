module BinaryTree.P68Spec (spec) where

import BinaryTree.BinaryTree (fromList)
import BinaryTree.P67A
import BinaryTree.P68
import Test.Hspec

spec :: Spec
spec = do
  describe "treeToPreorder" $ do
    it "constructs a preorder sequence" $ do
      let tree =
            fromList
              head
              [ "x",
                "y",
                "a",
                "c",
                "d",
                "null",
                "b",
                "null",
                "null",
                "f",
                "null",
                "null",
                "null",
                "g",
                "h"
              ]
      treeToPreorder tree `shouldBe` "xycdfghab"

  describe "treeToInorder" $ do
    it "constructs an inorder sequence" $ do
      let tree =
            fromList
              head
              [ "x",
                "y",
                "a",
                "c",
                "d",
                "null",
                "b",
                "null",
                "null",
                "f",
                "null",
                "null",
                "null",
                "g",
                "h"
              ]
      treeToInorder tree `shouldBe` "cygfhdxab"

  -- QuickCheck may generate a Tree with duplicates,
  -- which breaks the algorithm.
  describe "preInTree" $ do
    it "constructs a tree from preorder and inorder sequences" $ do
      let tree = stringToTree "a(b(d,e),c(,f(g,)))"
      let po = treeToPreorder tree
      let io = treeToInorder tree

      preInTree po io `shouldBe` tree
