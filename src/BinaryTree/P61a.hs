module BinaryTree.P61A where

import BinaryTree.BinaryTree (Tree (..))

-- Problem 61A: (*) Collect the leaves of a binary tree in a list.
leaves :: Tree a -> [a]
leaves = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = case (l, r) of
      (Empty, Empty) -> x : acc
      _ -> go (go acc r) l
