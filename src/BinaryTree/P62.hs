module BinaryTree.P62 where

import BinaryTree.BinaryTree (Tree (..))

-- Problem 62: (*) Collect the internal nodes of a binary tree in a list.
internals :: Tree a -> [a]
internals = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = case (l, r) of
      (Empty, Empty) -> acc
      _ -> x : go (go acc r) l
