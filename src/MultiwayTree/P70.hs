module MultiwayTree.P70 where

import DList (DList)
import qualified DList as D
import MultiwayTree.MultiwayTree (Tree (..))

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
