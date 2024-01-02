{-# LANGUAGE DerivingStrategies #-}

module MultiwayTrees
  ( Tree (..),
    nnodes,
    stringToTree,
    treeToString,
    ipl,
    bottomUp,
    treeToLisp,
    lispToTree,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Applicative as A
import DList (DList)
import qualified DList as D
import Parser (Parser (..))
import qualified Parser as P

data Tree a = Node a [Tree a]
  deriving stock (Eq, Show)

-- Problem 70B: (*) Check whether a given term represents a multiway tree.
-- ANSWER: Creating an invalid tree is not possible in Haskell.

-- Problem 70C: (*) Count the nodes of a multiway tree.
nnodes :: Tree a -> Int
nnodes (Node _ forest) = 1 + foldr ((+) . nnodes) 0 forest

{-
Problem 70: (**) Construct a multiway tree from a node string.

Suppose that the nodes of a multiway tree contain single characters.
In the depth-first order sequence of its nodes, a special character
'^' has been inserted whenever, during the tree traversal, the move
is a backtrack to the previous level.
By this rule, the tree above (tree5) is represented as: afg^^c^bd^e^^^

Write a predicate to construct the Tree when the String is given.
Make your predicate work in both directions.
-}
stringToTree :: String -> Tree Char
stringToTree = head . fst . dfs
  where
    {-
    First recursive dfs call collects the children,
    second recursive dfs call collects the siblings.

    x=g, xs=^^c^bd^e^^^, ys=^c^bd^e^^^, zs=c^bd^e^^^
    x=e, xs=^^^, ys=^^, zs=^
    x=d, xs=^e^^^, ys=e^^^, zs=^
    x=b, xs=d^e^^^, ys=^, zs=
    x=c, xs=^bd^e^^^, ys=bd^e^^^, zs=
    x=f, xs=g^^c^bd^e^^^, ys=c^bd^e^^^, zs=
    x=a, xs=fg^^c^bd^e^^^, ys=, zs=
    -}
    dfs :: String -> ([Tree Char], String)
    dfs [] = ([], [])
    dfs ('^' : xs) = ([], xs)
    dfs (x : xs) = (Node x children : siblings, zs)
      where
        (children, ys) = dfs xs
        (siblings, zs) = dfs ys

treeToString :: Tree Char -> String
treeToString = D.toList . go D.empty
  where
    go :: DList Char -> Tree Char -> DList Char
    go acc (Node x forest) = acc D.++ ys
      where
        xs = foldl go D.empty forest
        ys = D.singleton x D.++ xs D.++ D.singleton '^'

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

-- Problem 72: (*) Construct bottom-up order sequence of multiway tree nodes.
bottomUp :: Tree a -> [a]
bottomUp = go []
  where
    go acc (Node x forest) = foldr (flip go) (x : acc) forest

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
