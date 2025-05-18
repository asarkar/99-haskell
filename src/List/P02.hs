module List.P02 where

-- Problem 2: (*) Find the last-but-one (or second-last) element of a list.
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x, _] = x
myButLast xs = myButLast (tail xs)
