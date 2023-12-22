{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module Tree (Tree (..), fromList, toList) where

import qualified Data.List as L
import qualified Debug.Trace as T

data Tree a = Node a (Tree a) (Tree a) | Empty deriving stock (Show, Eq)

fromList :: (Show a) => (String -> a) -> [String] -> Tree a
fromList _ [] = Empty
fromList f xs = T.trace ("\n" ++ show xs ++ " ==> \n" ++ drawTree tree) tree
  where
    (_, tree : trees) = L.mapAccumL g (trees ++ repeat Empty) xs
    g ts "null" = (ts, Empty)
    g (l : r : ts) x = (ts, Node (f x) l r)

toList :: (Show a) => Tree a -> [String]
toList = L.dropWhileEnd (== "null") . concat . go . pure
  where
    go [] = []
    go nodes = thisL : go nextL
      where
        (thisL, nextL) = foldr f ([], []) nodes
        f Empty (this, next) = ("null" : this, next)
        f (Node x l r) (this, next) = (show x : this, l : r : next)

draw :: (Show a) => Tree a -> [String]
draw Empty = []
draw (Node x l r) = show x : drawSubTrees [l, r]
  where
    drawSubTrees [] = []
    drawSubTrees [Empty] = []
    drawSubTrees [t] =
      "|" : shift "`- " "   " (draw t)
    drawSubTrees (Empty : ts) = drawSubTrees ts
    drawSubTrees (t : ts) =
      "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

drawTree :: (Show a) => Tree a -> String
drawTree = unlines . draw
