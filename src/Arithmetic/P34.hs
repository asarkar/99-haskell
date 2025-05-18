module Arithmetic.P34 where

import Arithmetic.P35
import qualified Control.Monad as M

{-
Problem 34: (**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the
number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1
-}
totient :: Int -> Int
totient = round . M.liftM2 (*) fromIntegral (product . map f . primeFactors)
  where
    f :: Int -> Float
    f x = 1 - fromIntegral (1 :: Int) / fromIntegral x
