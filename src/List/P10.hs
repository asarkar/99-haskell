module List.P10 where

import Control.Arrow ((&&&))
import List.P09

-- Problem 10: (*) Run-length encoding of a list.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (length &&& head) . pack
