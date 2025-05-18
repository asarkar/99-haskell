{-# OPTIONS -Wno-orphans #-}
-- This is needed to implement Arbitrary for a Tree Int,
-- where Char is not a type variable.
{-# LANGUAGE FlexibleInstances #-}

module BinaryTree.BinaryTreeSpec (spec) where

import BinaryTree.BinaryTree (Tree (..), fromList, toList)
import BinaryTree.GenBinaryTree
import Test.Hspec
import Test.QuickCheck

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

instance Arbitrary (Tree Int) where
  arbitrary = do
    sz <- chooseInt (0, 100)
    let smallInt = arbitrary :: Gen (Small Int)
    genTree (getSmall <$> smallInt) sz

  shrink = shrinkTree

spec :: Spec
spec = do
  describe "Tree" $ do
    let t = Branch 5 (Branch 3 (Branch 2 (leaf 1) Empty) (leaf 4)) (leaf 6) :: Tree Int
    let ts = ["5", "3", "6", "2", "4", "null", "null", "1"]

    it "can be built from a list of strings" $ do
      (fromList read ts :: Tree Int) `shouldBe` t
      (fromList read [] :: Tree Int) `shouldBe` Empty

    it "can be converted to a list of strings" $ do
      toList show t `shouldBe` ts
      toList show (Empty :: Tree Int) `shouldBe` []

    it "fromList is the inverse of toList" $
      within 1000000 $
        \t' -> fromList read (toList show (t' :: Tree Int)) `shouldBe` t'
