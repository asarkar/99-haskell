module BinaryTree.P69 where

import BinaryTree.BinaryTree (Tree (..))
import qualified DList as D

{-
Problem 69: (**) Dotstring representation of binary trees.

First, try to establish a syntax (BNF or syntax diagrams)
and then write a predicate tree_dotstring which does the
conversion in both directions. Use difference lists.

ANSWER:
tree ::= empty | branch
branch ::= letter tree tree
empty ::= '.'
letter ::= 'a' ... 'z'
-}
tree2ds :: Tree Char -> String
tree2ds = D.toList . go
  where
    go Empty = D.singleton '.'
    go (Branch x l r) = D.singleton x D.++ go l D.++ go r

ds2tree :: String -> Tree Char
ds2tree = fst . go
  where
    go [] = (Empty, "")
    go ('.' : xs) = (Empty, xs)
    go (x : xs) = (Branch x left right, zs)
      where
        (left, ys) = go xs
        (right, zs) = go ys
