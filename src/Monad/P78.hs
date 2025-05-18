module Monad.P78 where

import Control.Monad.Writer.Strict (Writer)
import qualified Control.Monad.Writer.Strict as W
import Data.Monoid (Sum (..))
import qualified Data.Monoid as Md

{-
Problem 78: (*) Collatz conjecture.

Starting from a positive integer n, we can have a sequence of numbers such that at each step,
the next number is 3n + 1 if n  is odd, or n/2 if n is even. The Collatz conjecture states
that this sequence will always end at 1 after a finite number of steps.

Using the Writer monad, count the number of these steps for a given positive integer n.
-}

collatz :: Int -> Int
collatz = Md.getSum . W.execWriter . go
  where
    -- Writer w a, combines w using
    -- a Monoid instance for w.
    go :: Int -> Writer (Sum Int) ()
    go 1 = return ()
    go n = do
      W.tell 1
      if even n
        then go (n `div` 2)
        else go (3 * n + 1)
