module List.P23 (rndSelect) where

import qualified Control.Monad as M
import Data.Functor ((<&>))
import qualified System.Random as R

-- Problem 23: (**) Extract a given number of randomly selected elements from a list.
-- Note: This implementation chooses with replacement.
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
  | n < 0 = error "negative n"
  | null xs || n == 0 = pure []
  | otherwise = M.replicateM n (randomElem xs)

randomElem :: [a] -> IO a
-- <&> ==> flip fmap
randomElem xs = i <&> (xs !!)
  where
    n = length xs
    i = R.randomRIO (0, n - 1)
