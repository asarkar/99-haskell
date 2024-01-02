{-# OPTIONS -Wno-orphans #-}
-- This is needed to implement Arbitrary for a Tree Char,
-- where Char is not a type variable.
{-# LANGUAGE FlexibleInstances #-}

module MultiwayTreesSpec (spec) where

import GenMultiwayTree
import MultiwayTrees
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

tree4 :: Tree Char
tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 :: Tree Char
tree5 =
  Node
    'a'
    [ Node
        'f'
        [ Node 'g' []
        ],
      Node 'c' [],
      Node
        'b'
        [ Node 'd' [],
          Node 'e' []
        ]
    ]

{-
tree5:
           ┌──┐
 ┌─────────┤a ├────────┐
 │         └─┬┘        │
 │           │         │
 │           │         │
┌┴─┐       ┌─┴┐       ┌┴─┐
│f │       │c │  ┌────┤b ├─────┐
└┬─┘       └──┘  │    └──┘     │
 │               │             │
 │               │             │
┌┴─┐           ┌─┴┐           ┌┴─┐
│g │           │d │           │e │
└──┘           └──┘           └──┘

-}

t1 :: Tree Char
t1 =
  Node
    'a'
    [ Node 'f' [Node 'g' []],
      Node 'c' [],
      Node 'b' [Node 'd' [], Node 'e' []]
    ]

instance Arbitrary (Tree Char) where
  arbitrary = chooseInt (0, 100) >>= genTree (elements ['a' .. 'z'])
  shrink = shrinkTree

spec :: Spec
spec = do
  describe "nnodes" $ do
    it "counts the number of nodes" $ do
      nnodes (Node 'a' [Node 'b' []]) `shouldBe` 2
      nnodes t1 `shouldBe` 7

  describe "stringToTree" $ do
    it "constructs a multiway tree from a string" $ do
      let tree = stringToTree "afg^^c^bd^e^^^"
      tree `shouldBe` t1

  describe "treeToString" $ do
    it "converts a multiway tree to a string" $ do
      treeToString t1 `shouldBe` "afg^^c^bd^e^^^"

  describe "ipl" $ do
    it "calculates internal path length" $ do
      ipl tree5 `shouldBe` 9
      ipl tree4 `shouldBe` 2

  describe "bottomUp" $ do
    it "constructs the bottom-up sequence of the nodes" $ do
      bottomUp tree5 `shouldBe` "gfcdeba"

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
