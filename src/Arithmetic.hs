module Arithmetic where

import Control.Arrow ((&&&))
import qualified Control.Monad as M
import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU
import qualified Data.List as L

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

{-
Problem 32: (**) Determine the greatest common divisor of two positive integer numbers.

Use Euclid's algorithm.
-}
myGCD :: Int -> Int -> Int
myGCD x y
  | y == 0 = x
  | x < y = myGCD y x
  | otherwise = myGCD y (x `mod` y)

{-
Problem 33: (*) Determine whether two positive integer numbers are coprime.

Two numbers are coprime if their greatest common divisor equals 1.
-}
coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1

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

{-
Problem 36: (**) Determine the prime factors and their multiplicities
of a given positive integer.
-}
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (head &&& length) . L.group . primeFactors

{-
Problem 37: (**) Calculate Euler's totient function phi(m) (improved).

If the list of the prime factors of a number m is known, then
the function phi(m) can be efficiently calculated as follows:

Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors
(and their multiplicities) of a given number m. Then phi(m) can
be calculated with the following formula:

phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
         (p2 - 1) * p2 ** (m2 - 1) *
         (p3 - 1) * p3 ** (m3 - 1) * ...

Note that a ** b stands for the b'th power of a.

ANSWER:
Already solved above using this method.
https://www.youtube.com/watch?v=HgUfBx8Pvz0
-}

{-
Problem 38: (*) Compare the two methods of calculating Euler's totient function.

Use the solutions of Problems 34 and 37 to compare the algorithms.
Take the number of reductions as a measure for efficiency.
Try to calculate phi(10090) as an example.

ANSWER:
TODO. What's up with the obsession with Euler's totient function,
let's do something different FFS!
-}

{-
Problem 39: (*) A list of prime numbers in a given range.

Given a range of integers by its lower and upper limit,
construct a list of all prime numbers in that range.

ANSWER:
Copied from: https://wiki.haskell.org/Prime_numbers,
because this prime number $hit is getting boring.
-}
primesR :: Int -> Int -> [Int]
primesR a b = if a < 3 then 2 : xs else xs
  where
    xs = [i | i <- [o, o + 2 .. b], arr A.! i]
    o = max (a + fromEnum (even a)) 3 -- first odd in the segment
    r = floor . sqrt $ (fromIntegral b :: Float) + 1
    arr =
      AU.accumArray
        (\_ _ -> False) -- accumulating fn
        True -- initial value
        (o, b) -- bounds of the array
        [ (i, ()) -- association list
          | p <- [3, 5 .. r],
            let q = p * p -- flip every multiple of an odd to False
                s = 2 * p
                -- Difference between divMod and quotRem.
                -- https://stackoverflow.com/a/339823/839733
                (n, x) = quotRem (o - q) s
                q2 = if o <= q then q else q + (n + signum x) * s,
            i <- [q2, q2 + s .. b]
        ]

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

{-
Problem 41: (**) A list of even numbers and their Goldbach compositions in a given range.

Given a range of integers by its lower and upper limit,
print a list of all even numbers and their Goldbach composition.
-}
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList lo hi = [goldbach x | x <- [lo .. hi], even x]
