module Arithmetic.P40 where

import Arithmetic.P31
import Arithmetic.P39

{-
Problem 40: (**) Goldbach's conjecture.

Goldbach's conjecture says that every positive even number
greater than 2 is the sum of two prime numbers.
-}
goldbach :: Int -> (Int, Int)
goldbach n =
  head
    [ (x, y)
    | x <- primesR 2 (n - 2),
      let y = n - x,
      isPrime y
    ]
