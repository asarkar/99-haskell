{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module BinaryTree.BinaryTree (Tree (..), fromList, toList) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Debug.Trace as T

data Tree a = Branch a (Tree a) (Tree a) | Empty
  deriving stock (Eq)

instance (Show a) => Show (Tree a) where
  show = drawTree

{-
Builds a tree in level order, where empty nodes are represented
by the string "null" in the input list.
-}
fromList :: (Show a) => (String -> a) -> [String] -> Tree a
fromList _ [] = Empty
fromList f xs = T.trace ("\n" ++ show xs ++ " ==> \n" ++ drawTree tree) tree
  where
    (_, tree : trees) = L.mapAccumL g (trees ++ repeat Empty) xs
    g ts "null" = (ts, Empty)
    g (l : r : ts) x = (ts, Branch (f x) l r)

toList :: (a -> String) -> Tree a -> [String]
toList f = L.dropWhileEnd (== "null") . concat . go . pure
  where
    go [] = []
    go nodes = thisL : go nextL
      where
        (thisL, nextL) = foldr g ([], []) nodes
        g Empty (this, next) = ("null" : this, next)
        g (Branch x l r) (this, next) = (f x : this, l : r : next)

{-
Adapted from https://pypi.org/project/binarytree/.

Recursively walks down the binary tree and build a pretty-printing string.

In each recursive call, a "box" of characters visually representing the
current (sub)tree is constructed line by line. Each line is padded with
whitespaces to ensure all lines in the box have the same length. Then the
box, its width, and start-end positions of its root node value string
(required for drawing branches) are sent up to the parent call. The parent
call then combines its left and right sub-boxes to build a larger box etc.
-}
drawTree :: (Show a) => Tree a -> String
drawTree tree = "\n" ++ L.intercalate "\n" (map rStrip lines')
  where
    (lines', _, _, _) = draw tree 0
    rStrip = L.dropWhileEnd C.isSpace

draw :: (Show a) => Tree a -> Int -> ([String], Int, Int, Int)
draw Empty _ = ([], 0, 0, 0)
draw (Branch x l r) i = (box, length (head box), start, end)
  where
    (l1, l2, lBox, lWidth) = drawLeft l i
    (r1, r2, rBox, rWidth) = drawRight r i
    val = show x
    width = length val
    line1 = l1 ++ [val] ++ r1
    line2 = l2 ++ [blankN width] ++ r2
    gapSize w = if w > 0 then 1 else 0
    gap = blankN (width + gapSize lWidth + gapSize rWidth)
    start = if lWidth > 0 then lWidth + 1 else 0
    end = start + width - 1
    n = max (length lBox) (length rBox)
    lLine = take n (lBox ++ repeat (blankN lWidth))
    rLine = take n (rBox ++ repeat (blankN rWidth))
    box =
      [concat line1, concat line2]
        ++ zipWith (\ll rl -> ll ++ gap ++ rl) lLine rLine

blankN :: Int -> String
blankN n = replicate n ' '

drawLeft :: (Show a) => Tree a -> Int -> ([String], [String], [String], Int)
drawLeft tree i
  | width > 0 = (line1, line2, box, width)
  | otherwise = ([], [], [], 0)
  where
    (box, width, start, end) = draw tree (2 * i + 1)
    root = ((start + end) `div` 2) + 1
    line1 = [blankN (root + 1), replicate (width - root) '_']
    line2 = [blankN root ++ "/", blankN (width - root)]

drawRight :: (Show a) => Tree a -> Int -> ([String], [String], [String], Int)
drawRight tree i
  | width > 0 = (line1, line2, box, width)
  | otherwise = ([], [], [], 0)
  where
    (box, width, start, end) = draw tree (2 * i + 2)
    root = (start + end) `div` 2
    line1 = [replicate root '_', blankN (width - root + 1)]
    line2 = [blankN root ++ "\\", blankN (width - root)]
