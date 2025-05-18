module Logic.P49 where

import qualified Data.Bits as B
import qualified Text.Printf as P

{-
Problem 49: (**) Gray codes.

Given a number of bits n, generate a gray code for it.

For example, for n = 2, one gray code would be [00, 01, 11, 10].

For number of bits n, there are 2^n Gray codes, including zero.
Thus, the maximum Gray code is 2^n - 1. Since, by definition,
Gray codes differ only by 1 bit from their neighbors, the i-th
Gray code is given by the XOR of the i-th and the (i - 1)th bits
of the binary representation of i, where 0 <= i < 2^n.

For the example above, n = 2, the corresponding Gray codes are:
0 ^ 0 = 0 (00), 1 ^ 0 = 1 (01), 2 ^ 1 = 3 (11) and, 3 ^ 1 = 2 (10)
-}
gray :: Int -> [String]
gray n = [P.printf "%0*b" n (g i) | i <- [0 .. x - 1]]
  where
    x = 1 `B.shiftL` n :: Int -- 2 ^ n - 1
    g i = i `B.xor` (i `B.shiftR` 1) -- i xor (i `div` 2)
