module BinaryTree.P62B where

import BinaryTree.BinaryTree (Tree (..))

-- Problem 62B: (*) Collect the nodes at a given level in a list.
atLevel :: Tree a -> Int -> [a]
atLevel = go []
  where
    go acc Empty _ = acc
    go acc (Branch x l r) level
      | level == 1 = x : acc
      | otherwise = go (go acc r (level - 1)) l (level - 1)
