module BinaryTree.P57 (construct) where

import BinaryTree.BinaryTree (Tree (..))

{-
Problem 57: (**) Binary search trees.
-}
construct :: (Ord a) => [a] -> Tree a
construct = foldl (flip add) Empty

-- Inserts 'a' into a BST.
add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
  LT -> Branch y (add x l) r
  GT -> Branch y l (add x r)
  EQ -> t
