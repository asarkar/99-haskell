module List.P30 where

import qualified Data.List as L

{-
Problem 30: (**) Write a function to compute the nth Fibonacci number.

Consider the following matrix equation, where F(n) is the nth Fibonacci number:

\|x2| = |1 1|   |F(n+1)|
\|x1| = |1 0| x |F(n)  |

When written out as linear equations, this is equivalent to:

x2 = F(n+1) + F(n)
x1 = F(n+1)

So x2 = F(n+2) and x1 = F(n+1).
Together with the associativity of matrix multiplication, this means:

\|F(n+2)|   |1 1|   |F(n+1)|   |1 1|   |1 1|   |F(n)  |         |1 1|^n   |F(2)|
\|F(n+1)| = |1 0| x |F(n)  | = |1 0| x |1 0| x |F(n-1)| = ... = |1 0|   x |F(1)|

Take advantage of this to write a function which computes the nth Fibonacci number
with O(log n) multiplications.
Compare with the solution for Problems.P29.
-}
fibonacci' :: Int -> Int
fibonacci' n
  | n <= 2 = n - 1
  | otherwise = head $ head $ go (n - 2)
  where
    go i
      | i == 1 = xs
      | even i = mmult ys ys
      | otherwise = mmult xs $ mmult ys ys
      where
        xs = [[1, 1], [1, 0]]
        ys = go (i `div` 2)

-- https://rosettacode.org/wiki/Matrix_multiplication#Haskell
-- Not the most efficient though.
mmult :: (Num a) => [[a]] -> [[a]] -> [[a]]
mmult a b = [[sum $ zipWith (*) ar bc | bc <- L.transpose b] | ar <- a]
