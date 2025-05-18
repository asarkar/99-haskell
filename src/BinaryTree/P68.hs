{-# OPTIONS -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module BinaryTree.P68 where

import BinaryTree.BinaryTree (Tree (..))
import qualified Data.List.Split as LS

{-
Problem 68: (**) Preorder and inorder sequences of binary trees.

a) Write predicates preorder and inorder that construct the
   preorder and inorder sequence of a given binary tree,
   respectively.

b) Can you use preorder from problem part a) in the reverse
   direction; i.e. given a preorder sequence, construct a
   corresponding tree? If not, make the necessary arrangements.

c) If both the preorder sequence and the inorder sequence of
   the nodes of a binary tree are given, then the tree is
   determined unambiguously.
   Write a predicate pre_in_tree that does the job.
-}
treeToPreorder :: Tree Char -> String
treeToPreorder = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = x : go (go acc r) l

treeToInorder :: Tree Char -> String
treeToInorder = go []
  where
    go acc Empty = acc
    go acc (Branch x l r) = go (x : go acc r) l

{-
ANSWER: We take each element from the preorder and find it in the inorder.
This is the root value, the left sequence is the left tree, and the right
sequence is the right tree. We recurse on the left and the right subtrees.

In addition to returning the node, we also return the preorder string that
is yet to be processed.
-}
preInTree :: String -> String -> Tree Char
preInTree pre = fst . build pre
  where
    build :: String -> String -> (Tree Char, String)
    build po [] = (Empty, po)
    build (x : xs) [_] = (Branch x Empty Empty, xs)
    build (x : xs) io = (Branch x left right, zs)
      where
        [io', io''] = LS.splitOn [x] io
        (left, ys) = build xs io'
        (right, zs) = build ys io''
