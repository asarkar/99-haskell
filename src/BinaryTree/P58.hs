module BinaryTree.P58 where

import BinaryTree.BinaryTree (Tree (..))
import BinaryTree.P55
import BinaryTree.P56

{-
Problem 58: (**) Generate-and-test paradigm.

Apply the generate-and-test paradigm to construct all symmetric,
completely balanced binary trees with a given number of nodes.
-}
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree
