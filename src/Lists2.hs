{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module Lists2
  ( encodeModified,
    decodeModified,
    dupli,
    repli,
    dropEvery,
    split,
    slice,
    rotate,
    removeAt,
  )
where

import qualified Control.Monad as M
import qualified Data.List as L

data ListItem a = Single a | Multiple Int a
  deriving stock (Show)

{-
Problem 11: (*) Modified run-length encoding.

Modify the result of problem 10 in such a way
that if an element has no duplicates it is simply copied
into the result list. Only elements with duplicates are
transferred as (N E) lists.
-}
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map (M.liftM2 merge length head) . L.group
  where
    merge 1 x = Single x
    merge n x = Multiple n x

{-Problem 12: (**) Decode a run-length encoded list.

Given a run-length code list generated as specified
in problem 11. Construct its uncompressed version.
-}
decodeModified :: [ListItem a] -> [a]
decodeModified = concat . expand
  where
    expand [] = []
    expand (x : xs) = case x of
      Single y -> [y] : expand xs
      Multiple n y -> replicate n y : expand xs

{-
Problem 13: (**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data
compression method directly.
-}

-- Problem 14: (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli xs = repli xs 2 -- (take 2 . repeat =<<)

-- Problem 15: (**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n

-- Problem 16: (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (i, x) <- zip [1 ..] xs, i `mod` n /= 0]

{-
Problem 17: (*) Split a list into two parts;
the length of the first part is given.

Do not use any predefined predicates.
-}
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x : xs) n = (x : left, right)
  where
    (left, right) = split xs (n - 1)

-- Problem 18: (**) Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs start end
  | null right = right
  | otherwise = take (end - start + 1) right
  where
    (_, right) = split xs (start - 1)

-- Problem 19: (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs k = right ++ left
  where
    n = k `mod` length xs
    (left, right) = split xs n

-- Problem 20: (*) Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
  | null right = error "n too large"
  | otherwise = (head right, left ++ tail right)
  where
    (left, right) = split xs (n - 1)
