{-# OPTIONS -Wno-orphans #-}
-- This is needed to implement Arbitrary for a Tree Char,
-- where Char is not a type variable.
{-# LANGUAGE FlexibleInstances #-}

module BinaryTrees2Spec (spec) where

import BinaryTrees2
import GenTree
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Tree (Tree (..), fromList)

{- ORMOLU_DISABLE -}
tree4 :: Tree Int
tree4 = fromList read ["1", "2", "2", "4"]

tree64 :: Tree Char
tree64 = fromList head
  [
    "n","k","u","c","m",
    "p","null","a","h","null",
    "null","null","s","null","null",
    "g","null","q","null","e"
  ]

tree65 :: Tree Char
tree65 = fromList head 
  [
    "n","k","u","c",
    "m","p","null","a"
    ,"e","null","null","null",
    "q","null","null","d","g"
  ]

instance Arbitrary (Tree Char) where
  arbitrary = chooseInt (0, 100) >>= genTree (elements ['a'..'z'])
  shrink = shrinkTree

spec :: Spec
spec = do
  describe "countLeaves" $ do
    it "counts the leaves of a binary tree" $ do
      countLeaves tree4 `shouldBe` 2

  describe "leaves" $ do
    it "collects the leaves of a binary tree in a list" $ do
      leaves tree4 `shouldMatchList` [4,2]

  describe "internals" $ do
    it "collects the internal nodes of a binary tree in a list" $ do
      internals tree4 `shouldMatchList` [1, 2]

  describe "atLevel" $ do
    it "collects the nodes at a given level in a list" $ do
      atLevel tree4 2 `shouldMatchList` [2, 2]

  describe "completeBinaryTree" $ do
    it "constructs a complete binary tree" $ do
      let t1 = fromList head ["x", "x", "x", "x"]
      completeBinaryTree (4 :: Int) `shouldBe` t1

      let t2 = fromList head ["x"]
      completeBinaryTree (1 :: Int) `shouldBe` t2

      let t3 = fromList head ["x", "x", "x", "x", "x"]
      completeBinaryTree (5 :: Int) `shouldBe` t3

  describe "layout" $ do
    it "annotates each node according to inorder position and depth" $ do
        -- (traceShow (toList (:[]) t2) t2)
      layout tree64 `shouldBe`
        Branch ('n', (8, 1))
               (Branch ('k', (6, 2))
                       (Branch ('c', (2, 3))
                               (Branch ('a', (1, 4)) Empty Empty)
                               (Branch ('h', (5, 4))
                                       (Branch ('g', (4, 5))
                                               (Branch ('e', (3, 6)) Empty Empty)
                                               Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch ('m', (7, 3)) Empty Empty)
                )
                (Branch ('u', (12, 2))
                        (Branch ('p', (9, 3))
                                Empty
                                (Branch ('s', (11, 4))
                                        (Branch ('q', (10, 5)) Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

  describe "layout2" $ do
    it "annotates each node according to height and depth" $ do
      layout2 tree65 `shouldBe`
        Branch ('n', (15, 1))
                (Branch ('k', (7, 2))
                        (Branch ('c', (3, 3))
                                (Branch ('a', (1, 4)) Empty Empty)
                                (Branch ('e', (5, 4))
                                        (Branch ('d', (4, 5)) Empty Empty)
                                        (Branch ('g', (6, 5)) Empty Empty)
                                )
                        )
                        (Branch ('m', (11, 3)) Empty Empty)
                )
                (Branch ('u', (23, 2))
                        (Branch ('p', (19, 3))
                                Empty
                                (Branch ('q', (21, 4)) Empty Empty)
                        )
                        Empty
                )

  -- describe "layout3" $ do
  --   it "annotates each node according to width and depth" $ do
  --     layout3 tree65 `shouldBe`
  --       Branch ('n', (5, 1))
  --               (Branch ('k', (3, 2))
  --                       (Branch ('c', (2, 3))
  --                               (Branch ('a', (1, 4)) Empty Empty)
  --                               (Branch ('e', (3, 4))
  --                                       (Branch ('d', (2, 5)) Empty Empty)
  --                                       (Branch ('g', (4, 5)) Empty Empty)
  --                               )
  --                       )
  --                       (Branch ('m', (4, 3)) Empty Empty)
  --               )
  --               (Branch ('u', (7, 2))
  --                       (Branch ('p', (6, 3))
  --                               Empty
  --                               (Branch ('q', (7, 4)) Empty Empty)
  --                       )
  --                       Empty
  --               )

  describe "to and from tree" $ do
    -- it "stringToTree is the inverse of treeToString" $ do
      -- let t1 = fromList head ["x", "y", "a", "null", "null", "null", "b"]
      -- stringToTree (treeToString t1) `shouldBe` t1

      -- let t2 = fromList head 
      --            [
      --               "x","y","a","c","d","null",
      --               "b","null","null","f","null",
      --               "null","null","g","h"
      --            ]
      -- stringToTree (treeToString t2) `shouldBe` t2

    -- it "ds2tree is the inverse of tree2ds" $ do
    --   let tree = fromList head ["x","y","z", "null", "null", "0"]
    --   ds2tree (tree2ds tree) `shouldBe` tree

    -- \t -> conjoin [
    --   counterexample "stringToTree is the inverse of treeToString" $ prop_stringToTree t,
    --   counterexample "ds2tree is the inverse of tree2ds" $ prop_ds2tree t]

    prop "ds2tree is the inverse of tree2ds" $
      \tree -> ds2tree (tree2ds (tree :: Tree Char)) `shouldBe` tree

    prop "stringToTree is the inverse of treeToString" $
      \tree -> stringToTree (treeToString tree) `shouldBe` tree


  describe "treeToPreorder" $ do
    it "constructs a preorder sequence" $ do
      let tree = fromList head 
                   [
                      "x","y","a","c","d","null",
                      "b","null","null","f","null",
                      "null","null","g","h"
                   ]
      treeToPreorder tree `shouldBe` "xycdfghab"

  describe "treeToInorder" $ do
    it "constructs an inorder sequence" $ do
      let tree = fromList head 
                   [
                      "x","y","a","c","d","null",
                      "b","null","null","f","null",
                      "null","null","g","h"
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

  describe "tree2ds" $ do
    it "converts a tree to its Dotstring representation" $ do
      let tree = fromList head ["x","y","z", "null", "null", "0"]
      tree2ds tree `shouldBe` "xy..z0..."

  describe "ds2tree" $ do
    it "constructs a tree from its Dotstring representation" $ do
      let tree = fromList head 
                   [
                     "a","b","c","d","e", 
                     "null","f","null","null",
                     "null","null","g"
                   ]

      ds2tree "abd..e..c.fg." `shouldBe` tree
