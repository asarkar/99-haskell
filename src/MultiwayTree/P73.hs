module MultiwayTree.P73 where

import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import DList (DList)
import qualified DList as D
import MultiwayTree.MultiwayTree (Tree (..))
import Parser (Parser (..))
import qualified Parser as P

{-
Problem 73: (**) Lisp-like multiway tree representation.

An s-expression is commonly represented as a list of items between parentheses.
In particular, s-expressions can be nested, e.g., (a b (c d) e (f g)).
It is used by programming languages such as Lisp and Scheme.

A possible way to represent a multiway tree with an s-expression is for the
first element in a list to represent the root of the tree, and the rest of
the elements in the list would be its children.
As a special case, a multiway tree with no children is represented without parentheses.

Write a function which will return this s-expression representation of a
multiway tree as a string.

As a second, even more interesting exercise try to write the inverse conversion.
-}
treeToLisp :: Tree Char -> String
treeToLisp = tail . D.toList . go D.empty
  where
    go :: DList Char -> Tree Char -> DList Char
    go acc (Node x []) = acc D.++ D.singleton ' ' D.++ D.singleton x
    go acc (Node x forest) =
      acc
        D.++ D.singleton ' '
        D.++ D.singleton '('
        D.++ D.singleton x
        D.++ xs
        D.++ D.singleton ')'
      where
        xs = foldl go D.empty forest

type TreeParser = Parser (Tree Char)

branch :: TreeParser
branch = do
  x <- P.open *> P.letter <* P.space
  forest <- A.some mwTree <* P.close
  return $ Node x forest

singleton :: TreeParser
singleton = do
  x <- P.letter
  return $ Node x []

mwTree :: TreeParser
mwTree = do
  tree <- branch <|> singleton
  P.space
  return tree

lispToTree :: String -> Tree Char
lispToTree = fst . head . P.parse mwTree
