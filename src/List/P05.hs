module List.P05 where

-- Problem 5: (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
