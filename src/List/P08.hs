module List.P08 where

import Control.Monad ((<=<))
import List.P09

-- Problem 8: (**) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress = take 1 <=< pack
