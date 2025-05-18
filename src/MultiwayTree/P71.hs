module MultiwayTree.P71 where

import MultiwayTree.MultiwayTree (Tree (..))

{-
Problem 71: (*) Determine internal path length of multiway tree.
We define the internal path length of a multiway tree as the total
sum of the path lengths from the root to all nodes of the tree.
By this definition, tree5 has an internal path length of 9.

ANSWER: We observe that the path length is equal to the number of
nodes in a path from the root to a leaf, with a node counted only
once. So, path length of abd = 3, but abd + abe is 4, not 6.

The catch is to pass the _same_ accoumulated value to all the
children of a node.

-}

ipl :: Tree a -> Int
ipl = go 0
  where
    go acc (Node _ forest) = acc + sum (map (go (acc + 1)) forest)
