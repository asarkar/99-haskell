module BinaryTree.P65 (layout2) where

import BinaryTree.BinaryTree (Tree (..))

{-
Problem 65: (**) Layout algorithm for displaying trees (part 2).

ANSWER: In this problem, no two nodes share the same Y-coordinate.
Thus, the X-coordinate of a node is determined by the maximum
height of its subtrees. In order to avoid calculating the height of
the tree at every node, we calculate the height of the root tree first.

The nodes on the second level (children of root) are each separated
by 2 * height from the root, the nodes on the next level are
separated by half of the separation value on the level above,
and so on.

We start with the value 2 * height for the separator and halve it
each time when recurring on the children. The X-coordinate of a
node is given by the X-coordinate of its left child plus the
separation value. The X-coordinate of a right child is given by
the by the X-coordinate of its parent plus the separation value.
We also need to handle the special case for the leftmost node with
position 1.
-}
type Pos = (Int, Int)

type AnnotatedTree a = Tree (a, Pos)

height :: Tree a -> Int
height Empty = -1
height (Branch _ l r) = 1 + max (height l) (height r)

layout2 :: Tree a -> AnnotatedTree a
layout2 = fst . (go 1 1 =<< (2 *) . height)
  where
    go :: Int -> Int -> Int -> Tree a -> (AnnotatedTree a, Int)
    go _ _ _ Empty = (Empty, 0)
    go pos depth ht (Branch x l r) = (node, pos')
      where
        depth' = depth + 1
        ht' = ht `div` 2
        (left, lPos) = go (pos - ht) depth' ht' l
        pos' = if lPos > 0 then lPos + ht else max pos 1
        (right, _) = go (pos' + ht) depth' ht' r
        node = Branch (x, (pos', depth)) left right
