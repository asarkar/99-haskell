module Arithmetic.P32 where

{-
Problem 32: (**) Determine the greatest common divisor of two positive integer numbers.

Use Euclid's algorithm.
-}
myGCD :: Int -> Int -> Int
myGCD x y
  | y == 0 = x
  | x < y = myGCD y x
  | otherwise = myGCD y (x `mod` y)
