module Misc.P93 (puzzle) where

{-
Problem 93: (***) An arithmetic puzzle.

Given a list of positive integer numbers, find a correct way of inserting
the arithmetic signs such that the result is a correct equation.

For example, with the list of numbers [2,3,5,7,11], we can form the equations
2-3+5+7=11 or 2=(3*5+7)/11.

The arithmetic signs to insert are:

+ : addition
- : subtraction
\* : multiplication
/ : division
= : equality
(, ) : parentheses

Arithmetic operations are only binary, e.g., -4 should not be included.
Division should be interpreted as operating on rationals, e.g., 3/5=6/10
but 3/5≠0, and division by zero should be avoided.
Parentheses should be inserted only when the default precedence rules
need to be overridden. Equality should be inserted exactly once.

ANSWER: Adopted from the 99 questions Haskell Wiki.
https://wiki.haskell.org/index.php?title=99_questions/Solutions/93
-}

puzzle :: [Integer] -> [String]
puzzle xs = do
  i <- [1 .. length xs - 1]
  let (left, right) = splitAt i xs
  (sl, vl, _) <- gen left
  (sr, vr, _) <- gen right
  if vl == vr then return (sl ++ " = " ++ sr) else []

-- Return a list of tuples: (expression, evaluated value, last operator).
gen :: [Integer] -> [(String, Rational, Char)]
gen [x] = [(show x, fromInteger x, '_')]
gen xs = do
  i <- [1 .. length xs - 1]
  let (left, right) = splitAt i xs
  (sl, vl, opl) <- gen left
  (sr, vr, opr) <- gen right
  (op, f) <- operators
  if (op == '/' && vr == 0) || isDuplicate op opr
    then []
    else return (formatExpression sl sr op opl opr, f vl vr, op)

operators :: [(Char, Rational -> Rational -> Rational)]
operators = [('+', (+)), ('-', (-)), ('*', (*)), ('/', (/))]

-- Remove duplicates.
-- Example: 1 + 2 - 3 is produces by combining left=1 with right=(2 - 3),
-- and also by combining left=(1 + 2) with right=3.
-- We eliminate the 1st combination, as the 2nd combination will be produced
-- later for a different splitting point.
isDuplicate :: Char -> Char -> Bool
isDuplicate op opr
  | op == '+' && (opr == '+' || opr == '-') = True
  | op == '*' && (opr == '*' || opr == '/') = True
  | otherwise = False

formatExpression :: String -> String -> Char -> Char -> Char -> String
formatExpression sl sr op opl opr =
  let left = if needsParenthesesLeft op opl then "(" ++ sl ++ ")" else sl
      right = if needsParenthesesRight op opr then "(" ++ sr ++ ")" else sr
   in left ++ " " ++ [op] ++ " " ++ right

-- Parenthesize left expression if the last left operator is of lower precedence.
-- Example: (3 * 5 + 7) / 11
needsParenthesesLeft :: Char -> Char -> Bool
needsParenthesesLeft op opl = (op == '*' || op == '/') && (opl == '+' || opl == '-')

-- Parenthesize right expression if the right operator is of the same or lower precedence.
-- Example 3 - (5 + 7)
needsParenthesesRight :: Char -> Char -> Bool
needsParenthesesRight op opr =
  (op == '/' && opr /= '_') || ((op == '-' || op == '*') && (opr == '+' || opr == '-'))
