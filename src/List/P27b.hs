module List.P27b where

import Data.List ((\\))
import List.P26

{-
Problem 27b: (**) In how many ways can a group of 9 people
work in disjoint subgroups of the given sizes?
Write a function that generates all the possibilities
and returns them in a list.
-}
group :: (Eq a) => [Int] -> [a] -> [[[a]]]
group [] _ = []
group [_] xs = [[xs]]
group (i : is) xs = do
  ys <- combinations i xs
  xxs <- group is (xs \\ ys)
  return (ys : xxs)
