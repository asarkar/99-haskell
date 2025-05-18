module List.P06 where

import qualified Control.Monad as M
import List.P05

-- Problem 6: (*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = M.ap (==) myReverse
