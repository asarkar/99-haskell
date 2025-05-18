module Monad.P75 where

{-
Problem 75: (*) Maybe monad.

In Problem 74, askGoldbach could not output an error if the input was not a number
or it was not an even number greater than 2. We could implement a function which
returned Nothing when the input is not valid.

However, the implementation of maybeGoldbach above is a chain of conditional expressions.
It is not problematic in this particular case, but can make things awkward when there are
many conditions and successful operations that need to happen for a function to return a
Maybe value.

Take advantage of the fact that Maybe is a monad and rewrite maybeGoldbach more succintly
using do notation. The guard function, which in the Maybe monad returns Just () when its
argument is true and Nothing when its argument is false, would be useful for making it
even more succinct.
-}
