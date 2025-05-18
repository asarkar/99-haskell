module Misc.P95 where

import qualified Data.List as L

{-
Problem 95: (**) English number words.

On financial documents, like cheques, numbers must sometimes be written in full words.
Example: 175 must be written as one-seven-five.
Write a predicate full-words/1 to print (non-negative) integer numbers in full words.
-}
fullWords :: Int -> String
fullWords 0 = "zero"
fullWords x = (L.intercalate "-" . reverse) $ L.unfoldr go x
  where
    go :: Int -> Maybe (String, Int)
    go 0 = Nothing
    go n = (,left) <$> L.lookup right digit2Word
      where
        (left, right) = n `divMod` 10
    digit2Word =
      [ (1, "one"),
        (2, "two"),
        (3, "three"),
        (4, "four"),
        (5, "five"),
        (6, "six"),
        (7, "seven"),
        (8, "eight"),
        (9, "nine")
      ]
