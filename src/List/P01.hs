module List.P01 where

-- Problem 1: (*) Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast xs = myLast (tail xs)
