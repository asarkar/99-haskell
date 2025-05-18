module BinaryTree.P66 where

{-
Problem 66: (***) Layout algorithm for displaying trees (part 3).

The method yields a very compact layout while maintaining a
certain symmetry in every node. Find out the rules and write
the corresponding predicate.
Hint: Consider the horizontal distance between a node and its
successor nodes. How tight can you pack together two subtrees
to construct the combined binary tree?

ANSWER: We have to place the left and right subtrees so that
two nodes at the same level may not overlap, and that the
parent is evenly spaced between its children.

TODO.
-}
