module List.P28a where

import qualified Data.List as L

-- Problem 28a: (**) Sort the elements of this list according to their length;
-- i.e short lists first, longer lists later.
lsort :: [[a]] -> [[a]]
lsort = L.sortOn length
