module Arithmetic.P35 where

import Arithmetic.P31
import qualified Data.List as L

{-
Problem 35: (**) Determine the prime factors of a given positive integer.
-}
primeFactors :: Int -> [Int]
primeFactors = go primes
  where
    primes = L.unfoldr nxtPrime 1
    nxtPrime k = Just (prime, prime + 1)
      where
        prime = head $ dropWhile (not . isPrime) [k ..]
    go [] _ = error "no primes"
    go xs@(k : ys) x
      | x > 1 && x `mod` k == 0 = k : go xs (x `div` k)
      | x <= 1 = []
      | otherwise = go ys x
