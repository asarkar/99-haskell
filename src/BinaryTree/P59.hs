module BinaryTree.P59 (hbalTree) where

import BinaryTree.BinaryTree (Tree (..))

{-
Problem 59: (***) Construct height-balanced binary trees.

Construct a list of all height-balanced binary trees with
the given element and the given maximum height.
-}
hbalTree :: a -> Int -> [Tree a]
hbalTree x n
  | n == 0 = []
  | n == 1 = [singleton x]
  | n == 2 =
      [ Branch x (singleton x) Empty,
        Branch x Empty (singleton x),
        Branch x (singleton x) (singleton x)
      ]
  | otherwise = do
      {-
      Either the subtrees are of equal height,
      or differ by 1. Generate those combinations.
      -}
      xs <- hbalTree x (n - 1)
      ys <- hbalTree x (n - 2)
      [Branch x xs ys, Branch x ys xs, Branch x xs xs]

singleton :: a -> Tree a
singleton x = Branch x Empty Empty
