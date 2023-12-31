{-# OPTIONS -Wno-incomplete-uni-patterns #-}

module Lists3
  ( insertAt,
    range,
    rndSelect,
    diffSelect,
    rndPerm',
    rndPerm,
    combinations,
    group3,
    group,
    lsort,
    lfsort,
    fibonacci,
    mmult,
    fibonacci',
  )
where

import qualified Control.Monad as M
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as AIO
import Data.Functor ((<&>))
import Data.List ((\\))
import qualified Data.List as L
import qualified System.Random as R

-- Problem 21: (*) Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
  | n > 0 = left ++ [x] ++ right
  | otherwise = error "invalid position"
  where
    (left, right) = L.splitAt (n - 1) xs

-- Problem 22: (*) Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range start end
  | start <= end = [start .. end]
  | otherwise = [start, start - 1 .. end]

randomElems :: Int -> Int -> [a] -> IO [a]
randomElems n m xs
  | n <= 0 = pure []
  | n > m = pure xs
  | otherwise = do
      k <- R.randomRIO (0, m - 1)
      let (left, x : right) = L.splitAt (k - 1) xs
      (x :) <$> randomElems (n - 1) (m - 1) (left ++ right)

-- Problem 23: (**) Extract a given number of randomly selected elements from a list.
-- Note: This implementation chooses with replacement.
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
  | n < 0 = error "negative n"
  | null xs || n == 0 = pure []
  | otherwise = M.replicateM n (randomElem xs)

randomElem :: [a] -> IO a
-- <&> ==> flip fmap
randomElem xs = i <&> (xs !!)
  where
    n = length xs
    i = R.randomRIO (0, n - 1)

-- Problem 24: (*) Draw N different random numbers from the set 1..M.
-- Note: The selected elements are unique.
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = randomElems n m [1 .. m]

-- Problem 25: (*) Generate a random permutation of the elements of a list.
rndPerm' :: [a] -> IO [a]
rndPerm' xs = randomElems n n xs
  where
    n = length xs

-- Problem 25: (*) Generate a random permutation of the elements of a list.

{-
ANSWER: Alternative imperative solution.

https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle

We iterate n times, and at each iteration read a random
element arr[j], where i <= j <= n, and overwrite it with
the current element arr[i].
We then return arr[j], which becomes the i-th element of
the output array. Note that there's no need to write
arr[j] at index i, since it is never visited again.
-}
rndPerm :: [a] -> IO [a]
rndPerm xs = do
  arr <- newArray xs

  M.forM [1 .. n] $ \i -> do
    j <- R.randomRIO (i, n)
    vi <- AIO.readArray arr i
    vj <- AIO.readArray arr j
    AIO.writeArray arr j vi
    return vj
  where
    n = length xs
    newArray :: [a] -> IO (IOArray Int a)
    newArray = AIO.newListArray (1, n)

-- Problem 26: (**) Generate the combinations of K distinct
-- objects chosen from the N elements of a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = return []
combinations n xs = do
  y : ys <- L.tails xs
  zs <- combinations (n - 1) ys
  return (y : zs)

{-
Problem 27a: (**) In how many ways can a group of 9 people
work in 3 disjoint subgroups of 2, 3 and 4 persons?
Write a function that generates all the possibilities
and returns them in a list.
-}
group3 :: (Eq a) => [a] -> [[[a]]]
group3 = group [2 .. 4]

{-
Problem 27b: (**) In how many ways can a group of 9 people
work in disjoint subgroups of the given sizes?
Write a function that generates all the possibilities
and returns them in a list.
-}
group :: (Eq a) => [Int] -> [a] -> [[[a]]]
group [] _ = []
group [_] xs = [[xs]]
group (i : is) xs = do
  ys <- combinations i xs
  xxs <- group is (xs \\ ys)
  return (ys : xxs)

-- Problem 28a: (**) Sort the elements of this list according to their length;
-- i.e short lists first, longer lists later.
lsort :: [[a]] -> [[a]]
lsort = L.sortOn length

-- Problem 28b: (**) Sort the elements of this list according to their length frequency;
-- i.e., lists with rare lengths are placed first, others with a more frequent length come later.
lfsort :: [[a]] -> [[a]]
lfsort xxs = L.sortOn lenFreq xxs
  where
    ls = map length xxs
    count x = (length . filter (== x)) ls
    lenFreq xs = count (length xs)

{-
Problem 29: (*) Write a function to compute the nth Fibonacci number.
-}
fibonacci :: Int -> Int
fibonacci = go 0 1
  where
    go x _ 1 = x
    go x y n = go y (x + y) (n - 1)

-- https://rosettacode.org/wiki/Matrix_multiplication#Haskell
-- Not the most efficient though.
mmult :: (Num a) => [[a]] -> [[a]] -> [[a]]
mmult a b = [[sum $ zipWith (*) ar bc | bc <- L.transpose b] | ar <- a]

{-
Problem 30: (**) Write a function to compute the nth Fibonacci number.

Consider the following matrix equation, where F(n) is the nth Fibonacci number:

\|x2| = |1 1|   |F(n+1)|
\|x1| = |1 0| x |F(n)  |

When written out as linear equations, this is equivalent to:

x2 = F(n+1) + F(n)
x1 = F(n+1)

So x2 = F(n+2) and x1 = F(n+1).
Together with the associativity of matrix multiplication, this means:

\|F(n+2)|   |1 1|   |F(n+1)|   |1 1|   |1 1|   |F(n)  |         |1 1|^n   |F(2)|
\|F(n+1)| = |1 0| x |F(n)  | = |1 0| x |1 0| x |F(n-1)| = ... = |1 0|   x |F(1)|

Take advantage of this to write a function which computes the nth Fibonacci number
with O(log n) multiplications.
Compare with the solution for Problems.P29.
-}
fibonacci' :: Int -> Int
fibonacci' n
  | n <= 2 = n - 1
  | otherwise = head $ head $ go (n - 2)
  where
    go i
      | i == 1 = xs
      | even i = mmult ys ys
      | otherwise = mmult xs $ mmult ys ys
      where
        xs = [[1, 1], [1, 0]]
        ys = go (i `div` 2)
