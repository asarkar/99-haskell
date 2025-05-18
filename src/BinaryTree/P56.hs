module BinaryTree.P56 (symmetric) where

import BinaryTree.BinaryTree (Tree (..))

{-
Problem 56: (**) Symmetric binary trees.

A binary tree is symmetric if you can draw a vertical line
through the root node and then the right subtree is the
mirror image of the left subtree.
-}
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror t1 t2 = case (t1, t2) of
  (Empty, Empty) -> True
  (Empty, _) -> False
  (_, Empty) -> False
  (Branch _ l1 r1, Branch _ l2 r2) -> mirror l1 r2 && mirror r1 l2
