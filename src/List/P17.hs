module List.P17 where

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
