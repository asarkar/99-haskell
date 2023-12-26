module DList where

newtype DList a = DList ([a] -> [a])

-- O(1)
empty :: DList a
empty = DList id

-- O(1)
singleton :: a -> DList a
singleton = DList . (:)

-- O(n)
fromList :: [a] -> DList a
fromList = DList . (Prelude.++)

-- O(1)
(++) :: DList a -> DList a -> DList a
(++) (DList d1) (DList d2) = DList (d1 . d2)

-- O(1)
toList :: DList a -> [a]
toList (DList d) = d []
