{-# OPTIONS -Wno-orphans #-}
-- This is needed to implement Arbitrary for a Tree Char,
-- where Char is not a type variable.
{-# LANGUAGE FlexibleInstances #-}

module BinaryTree.P67aSpec (spec) where

import BinaryTree.BinaryTree (Tree (..), fromList)
import BinaryTree.GenBinaryTree
import BinaryTree.P67a
import BinaryTree.P69
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary (Tree Char) where
  arbitrary = chooseInt (0, 100) >>= genTree (elements ['a' .. 'z'])
  shrink = shrinkTree

spec :: Spec
spec = do
  describe "stringToTree" $ do
    it "constructs a binary tree from a string" $ do
      let tree = fromList head ["x", "y", "a", "null", "null", "null", "b"]
      stringToTree "x(y,a(,b))" `shouldBe` tree

  describe "treeToString" $ do
    it "converts a binary tree to a string" $ do
      let t1 = fromList head ["x", "y", "a", "null", "null", "null", "b"]
      treeToString t1 `shouldBe` "x(y,a(,b))"

      let t2 = fromList head ["a", "a"]
      treeToString t2 `shouldBe` "a(a,)"

  describe "to and from tree" $ do
    -- \t -> conjoin [
    --   counterexample "stringToTree is the inverse of treeToString" $ prop_stringToTree t,
    --   counterexample "ds2tree is the inverse of tree2ds" $ prop_ds2tree t]

    prop "ds2tree is the inverse of tree2ds" $
      \tree -> ds2tree (tree2ds (tree :: Tree Char)) `shouldBe` tree

    prop "stringToTree is the inverse of treeToString" $
      \tree -> stringToTree (treeToString tree) `shouldBe` tree
