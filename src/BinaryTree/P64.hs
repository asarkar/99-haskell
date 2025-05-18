module BinaryTree.P64 where

import BinaryTree.BinaryTree (Tree (..))

{-
Problem 64: (**) Layout algorithm for displaying trees.
In this layout strategy, the position of a node v is obtained by the following two rules:

- x(v) is equal to the position of the node v in the inorder sequence
- y(v) is equal to the depth of the node v in the tree

Write a function to annotate each node of the tree with a position,
where (1,1) in the top left corner or the rectangle bounding the drawn tree.
-}
type Pos = (Int, Int)

type AnnotatedTree a = Tree (a, Pos)

layout :: Tree a -> AnnotatedTree a
layout = fst . go 0 1
  where
    -- Returns the modified tree and the number of nodes in it.
    go :: Int -> Int -> Tree a -> (AnnotatedTree a, Int)
    go _ _ Empty = (Empty, 0)
    go pos depth (Branch x l r) = (node, lSize + rSize + 1)
      where
        (left, lSize) = go pos (depth + 1) l
        pos' = lSize + pos + 1
        (right, rSize) = go pos' (depth + 1) r
        node = Branch (x, (pos', depth)) left right
