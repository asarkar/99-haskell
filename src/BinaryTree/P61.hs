module BinaryTree.P61 where

import BinaryTree.BinaryTree (Tree (..))

-- Problem 61: (*) Count the leaves of a binary tree.
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ l r) = case (l, r) of
  (Empty, Empty) -> 1
  _ -> countLeaves l + countLeaves r
