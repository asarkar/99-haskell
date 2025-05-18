module List.P27a where

import List.P27b

{-
Problem 27a: (**) In how many ways can a group of 9 people
work in 3 disjoint subgroups of 2, 3 and 4 persons?
Write a function that generates all the possibilities
and returns them in a list.
-}
group3 :: (Eq a) => [a] -> [[[a]]]
group3 = group [2 .. 4]
