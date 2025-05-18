{-# OPTIONS -Wno-orphans #-}
-- This is needed to implement Arbitrary for a Tree Char,
-- where Char is not a type variable.
{-# LANGUAGE FlexibleInstances #-}

module MultiwayTree.P73Spec (spec) where

import MultiwayTree.GenMultiwayTree
import MultiwayTree.MultiwayTree (Tree (..))
import MultiwayTree.P70
import MultiwayTree.P73
import MultiwayTree.Trees
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary (Tree Char) where
  arbitrary = chooseInt (0, 100) >>= genTree (elements ['a' .. 'z'])
  shrink = shrinkTree

spec :: Spec
spec = do
  describe "treeToLisp" $ do
    it "converts a multiway tree to a lispy string" $ do
      treeToLisp tree5 `shouldBe` "(a (f g) c (b d e))"

  describe "lispToTree" $ do
    it "constructs a multiway tree from a lispy string" $ do
      lispToTree "(a (f g) c (b d e))" `shouldBe` tree5

  describe "to and from tree" $ do
    prop "lispToTree is the inverse of treeToLisp" $
      \tree -> lispToTree (treeToLisp tree) `shouldBe` tree

    prop "stringToTree is the inverse of treeToString" $
      \tree -> stringToTree (treeToString tree) `shouldBe` tree
