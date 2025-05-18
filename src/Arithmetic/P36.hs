module Arithmetic.P36 where

import Arithmetic.P35
import Control.Arrow ((&&&))
import qualified Data.List as L

{-
Problem 36: (**) Determine the prime factors and their multiplicities
of a given positive integer.
-}
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (head &&& length) . L.group . primeFactors
