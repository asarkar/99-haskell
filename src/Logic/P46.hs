module Logic.P46 where

{-
Problem 46: (**) Truth tables for logical expressions.

Write a predicate table/3 which prints the truth table
of a given logical expression in two variables.
-}
table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = [(a, b, f a b) | a <- [True, False], b <- [True, False]]
