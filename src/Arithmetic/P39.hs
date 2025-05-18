module Arithmetic.P39 where

import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU

{-
Problem 39: (*) A list of prime numbers in a given range.

Given a range of integers by its lower and upper limit,
construct a list of all prime numbers in that range.

ANSWER:
Code copied from https://wiki.haskell.org/Prime_numbers,
explanation my own.
-}
primesR :: Int -> Int -> [Int]
primesR a b = if a < 3 then 2 : xs else xs
  where
    xs = [i | i <- [o, o + 2 .. b], arr A.! i]
    -- First odd in the segment.
    o = max (a + fromEnum (even a)) 3
    r = floor . sqrt $ (fromIntegral b :: Float) + 1
    arr =
      -- Create a boolean array indexed from o to b;
      -- the indices produced by the association list
      -- are set to false.
      AU.accumArray
        (\_ _ -> False) -- accumulating fn
        -- Default value, used when the index
        -- is not present in the association list.
        True
        (o, b) -- bounds of the array
        [ (i, ()) -- association list
        | p <- [3, 5 .. r],
          -- Flip every multiple of an odd to False.
          let q = p * p
              s = 2 * p
              -- Difference between divMod and quotRem.
              -- https://stackoverflow.com/a/339823/839733
              (n, x) = quotRem (o - q) s
              q2 = if o <= q then q else q + (n + signum x) * s,
          i <- [q2, q2 + s .. b]
        ]
