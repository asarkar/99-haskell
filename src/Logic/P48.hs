module Logic.P48 where

import qualified Control.Monad as M

{-
Problem 48: (*) Truth tables for logical expressions (part 3).

Generalize Problem 47 in such a way that the logical expression
may contain any number of logical variables.
-}
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = map (M.ap (++) (pure . f)) $ M.replicateM n [True, False]
