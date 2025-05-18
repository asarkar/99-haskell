module Arithmetic.P33 where

import Arithmetic.P32

{-
Problem 33: (*) Determine whether two positive integer numbers are coprime.

Two numbers are coprime if their greatest common divisor equals 1.
-}
coprime :: Int -> Int -> Bool
coprime = ((1 ==) .) . myGCD
