module Arithmetic.P41 where

import Arithmetic.P40

{-
Problem 41: (**) A list of even numbers and their Goldbach compositions in a given range.

Given a range of integers by its lower and upper limit,
print a list of all even numbers and their Goldbach composition.
-}
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList lo hi = [goldbach x | x <- [lo .. hi], even x]
