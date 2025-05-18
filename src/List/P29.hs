module List.P29 where

{-
Problem 29: (*) Write a function to compute the nth Fibonacci number.
-}
fibonacci :: Int -> Int
fibonacci = go 0 1
  where
    go x _ 1 = x
    go x y n = go y (x + y) (n - 1)
