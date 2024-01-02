{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS -Wno-incomplete-patterns #-}

module Lists
  ( NestedList (..),
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    flatten,
    compress,
    pack,
    encode,
  )
where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import qualified Control.Monad as M
import qualified Data.List as L

-- Problem 1: (*) Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast xs = myLast (tail xs)

-- Problem 2: (*) Find the last-but-one (or second-last) element of a list.
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x, _] = x
myButLast xs = myButLast (tail xs)

-- Problem 3: (*) Find the K'th element of a list.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt (x : _) 1 = x
elementAt xs k = elementAt (tail xs) (k - 1)

-- Problem 4: (*) Find the number of elements in a list.
myLength :: [a] -> Int
myLength = foldl (const . succ) 0

-- Problem 5: (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6: (*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = M.ap (==) myReverse

data NestedList a = Elem a | List [NestedList a]
  deriving stock (Show)

instance Foldable NestedList where
  foldr :: (a -> b -> b) -> b -> NestedList a -> b
  foldr f acc x = case x of
    Elem y -> f y acc
    List xs -> L.foldr (flip (foldr f)) acc xs

-- Problem 7: (**) Flatten a nested list structure.
flatten :: NestedList a -> [a]
flatten = foldr (:) []

-- Problem 8: (**) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress = take 1 <=< pack

-- Problem 9: (**) Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack = foldr go []
  where
    go x [] = [[x]]
    go x xxs@(ys@(y : _) : zs)
      | x == y = (x : ys) : zs
      | otherwise = [x] : xxs

-- Problem 10: (*) Run-length encoding of a list.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (length &&& head) . pack
