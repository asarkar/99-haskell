module List.P12 where

import List.P11 (ListItem (..))

{-Problem 12: (**) Decode a run-length encoded list.

Given a run-length code list generated as specified
in problem 11. Construct its uncompressed version.
-}
decodeModified :: [ListItem a] -> [a]
decodeModified = concat . expand
  where
    expand [] = []
    expand (x : xs) = case x of
      Single y -> [y] : expand xs
      Multiple n y -> replicate n y : expand xs
