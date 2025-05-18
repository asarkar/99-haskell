module Arithmetic.P42 (multiplicativeInverse) where

{-
Problem 42: (**) Modular multiplicative inverse.

In modular arithmetic, integers a and b being congruent modulo an integer n,
means that a - b = k * n, for some integer k.
Many of the usual rules for addition, subtraction, and multiplication in
ordinary arithmetic also hold for modular arithmetic.

A multiplicative inverse of an integer a modulo n is an integer x such that
ax is congruent to 1 with respect to n. It exists if and only if a and n
are coprime.

Write a function to compute the multiplicative inverse x of a given integer a
and modulus n lying in the range 0 <= x < n.
Use the extended Euclidean algorithm.

https://brilliant.org/wiki/extended-euclidean-algorithm/
-}
multiplicativeInverse :: (Integral a) => a -> a -> Maybe a
multiplicativeInverse a n
  | a >= n = multiplicativeInverse (a `mod` n) n
  | r == 1 = Just $ x `mod` n
  | otherwise = Nothing
  where
    (r, x, _) = reduce (n, a) (0, 1) (1, 0)

reduce :: (Integral a) => (a, a) -> (a, a) -> (a, a) -> (a, a, a)
reduce (0, r') (_, x') (_, y') = (r', x', y')
reduce (r, r') (x, x') (y, y') = reduce (r' - q * r, r) (x' - q * x, x) (y' - q * y, y)
  where
    q = r' `div` r
