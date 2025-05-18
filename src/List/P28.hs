module List.P28 where

import qualified Data.List as L

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
