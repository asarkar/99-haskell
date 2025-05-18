module List.P03 where

-- Problem 3: (*) Find the K'th element of a list.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt (x : _) 1 = x
elementAt xs k = elementAt (tail xs) (k - 1)
