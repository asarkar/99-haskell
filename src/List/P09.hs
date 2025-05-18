{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS -Wno-incomplete-patterns #-}

module List.P09 where

-- Problem 9: (**) Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack = foldr go []
  where
    go x [] = [[x]]
    go x xxs@(ys@(y : _) : zs)
      | x == y = (x : ys) : zs
      | otherwise = [x] : xxs
