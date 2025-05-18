module Arithmetic.P31 where

{-
Problem 31: (**) Determine whether a given integer number is prime.
-}
isPrime :: Int -> Bool
isPrime n
  | n == 2 || n == 3 = True
  | n <= 1 || even n || n `mod` 3 == 0 = False
  | otherwise = all prime [5, 11 .. i]
  where
    i = floor $ sqrt (fromIntegral n :: Float)
    prime j = (n `mod` j) /= 0 && (n `mod` (j + 2)) /= 0
