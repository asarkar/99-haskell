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
fromList = DList . (++)

-- O(1)
append :: DList a -> DList a -> DList a
append (DList d1) (DList d2) = DList (d1 . d2)

-- O(1)
toList :: DList a -> [a]
toList (DList d) = d []
