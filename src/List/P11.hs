{-# LANGUAGE DerivingStrategies #-}

module List.P11 where

import qualified Control.Monad as M
import qualified Data.List as L

{-
Problem 11: (*) Modified run-length encoding.

Modify the result of problem 10 in such a way
that if an element has no duplicates it is simply copied
into the result list. Only elements with duplicates are
transferred as (N E) lists.
-}

data ListItem a = Single a | Multiple Int a
  deriving stock (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map (M.liftM2 merge length head) . L.group
  where
    merge 1 x = Single x
    merge n x = Multiple n x
