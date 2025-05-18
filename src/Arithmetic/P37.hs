module Arithmetic.P37 where

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
