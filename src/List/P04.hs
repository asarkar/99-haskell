module List.P04 where

-- Problem 4: (*) Find the number of elements in a list.
myLength :: [a] -> Int
myLength = foldl (const . succ) 0
