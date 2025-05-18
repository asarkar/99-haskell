module List.P25 where

import qualified Control.Monad as M
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as AIO
import List.P24 (randomElems)
import qualified System.Random as R

-- Problem 25: (*) Generate a random permutation of the elements of a list.
rndPerm' :: [a] -> IO [a]
rndPerm' xs = randomElems n n xs
  where
    n = length xs

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
