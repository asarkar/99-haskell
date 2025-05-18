{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module Logic.P50 where

import qualified Data.Bifunctor as Bf
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as Q

{-
Problem 50: (***) Given a list of characters and their number of occurrences,
construct a list of the characters and their Huffman encoding.

ANSWER:

https://brilliant.org/wiki/huffman-encoding/

The Huffman coding algorithm takes in information about the frequencies or probabilities
of a particular symbol occurring. It begins to build the prefix tree from the bottom up,
starting with the two least probable symbols in the list. It takes those symbols and
forms a subtree containing them, and then removes the individual symbols from the list.
The algorithm sums the probabilities of elements in a subtree and adds the subtree and
its probability to the list. Next, the algorithm searches the list and selects the two
symbols or subtrees with the smallest probabilities. It uses those to make a new subtree,
removes the original subtrees/symbols from the list, and then adds the new subtree and
its combined probability to the list. This repeats until there is one tree and all
elements have been added.
-}

data HTree = Branch HTree HTree | Leaf Char
  deriving stock (Eq, Show)

type PQ = HashPSQ String Int HTree

huffman :: [(Char, Int)] -> [(Char, String)]
huffman freq = treeToList $ go $ Q.fromList initial
  where
    initial = map (\(c, n) -> ([c], n, Leaf c)) freq

    minN :: Int -> PQ -> ([(String, Int, HTree)], PQ)
    minN 0 q = ([], q)
    minN n q = case Q.minView q of
      Nothing -> ([], q)
      Just (k, p, v, q') ->
        let (xs, q'') = minN (n - 1) q'
         in ((k, p, v) : xs, q'')

    go :: PQ -> HTree
    go q = case Q.size q of
      0 -> error "empty heap"
      1 -> let ((_, _, tree) : _, _) = minN 1 q in tree
      _ -> do
        let ((k1, p1, v1) : (k2, p2, v2) : _, q') = minN 2 q
        let node =
              if p1 < p2
                then Branch v1 v2
                else Branch v2 v1
        go $ Q.insert (k1 ++ k2) (p1 + p2) node q'

    {-
    Although Huffman coding doesn't specify which of the
    two values to put on which side (i.e., left or right),
    the examples put the smaller value on the left.
    Also, the examples encode the left branch as 0.
    -}
    treeToList :: HTree -> [(Char, String)]
    treeToList (Leaf x) = [(x, "")]
    treeToList (Branch l r) = left ++ right
      where
        left = map (Bf.second ('0' :)) $ treeToList l
        right = map (Bf.second ('1' :)) $ treeToList r
