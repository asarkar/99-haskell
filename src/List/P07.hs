{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module List.P07 where

import qualified Data.List as L

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
