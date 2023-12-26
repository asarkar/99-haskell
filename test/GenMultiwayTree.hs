-- This is needed to convince the compiler that an
-- Arbitrary (Tree a) will be supplied in due time.
{-# LANGUAGE FlexibleContexts #-}

module GenMultiwayTree where

import MultiwayTrees (Tree (..))
import Test.QuickCheck

genTree :: Gen a -> Int -> Gen (Tree a)
genTree g size = frequency [(10, genNode size), (1, genNode 0)]
  where
    genNode n = do
      x <- g
      k <- chooseInt (0, n)
      let n' = if k > 0 then n `div` k else 0
      forest <- vectorOf k $ genNode n'
      return $ Node x forest

shrinkTree :: (Arbitrary a, Arbitrary (Tree a)) => Tree a -> [Tree a]
shrinkTree (Node _ []) = []
shrinkTree (Node x forest) =
  [Node x []]
    ++ forest
    ++ [Node x' xs | (x', xs) <- shrink (x, forest)]
