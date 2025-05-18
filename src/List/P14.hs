module List.P14 where

import List.P15

-- Problem 14: (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli xs = repli xs 2 -- (take 2 . repeat =<<)
