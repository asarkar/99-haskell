{-# LANGUAGE DerivingStrategies #-}

module Monads where

import qualified Control.Monad as M
import qualified Control.Monad.Identity as Id
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W
import qualified Data.Maybe as Mb
import Data.Monoid (Sum (..))
import qualified Data.Monoid as Md

-- import Control.Monad.Trans.Maybe (MaybeT)

{-
Problem 74: (**) Monads without do notation.

We would like to implement a function which reads an even number from standard input,
finds two prime numbers which add up to the number (see Problem 40), and prints out the
equation to standard output.

Implement the function without do notation. In other words, use >>= or >> directly,
instead of using them implicitly through do notation.
Try to use these functions with prefix style instead of infix style.
-}

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

{-
Problem 76: (*) Either monad.

In Problem 75, maybeGoldbach returned Nothing when there is an error. However, this revealed
nothing about why there is an error.

Rewrite maybeGoldbach to return an Either value,
-}

{-
Problem 77: (*) List monad.

Using the list monad, implement a function which returns all the one-dimensional random walk
paths with n steps. Starting from position 0, each step can change positions by -1, 0, or 1.
Each path will be a list of positions starting from 0.

ANSWER:
ReplicateM creates the cross-product of the given list n times.

Example:
  n=2, [[-1,-1], [-1,0], [-1,1], [0,-1], [0,0], [0,1], [1,-1], [1,0], [1,1]].
  Each of these represents the next step taken, so [-1,-1] means 2 steps from 0,
  0, -1 and -1. The position at each step is given by the sum of itself with
  the previous position, i.e. the cumulative sum.
  So, 0, -1 and -1 ==> 0, -1, -2.
-}

randomWalkPaths :: Int -> [[Int]]
randomWalkPaths 0 = [[0]]
randomWalkPaths n = map (scanl (+) 0) $ M.replicateM n [-1, 0, 1]

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
    -- Writer w a, where there exists
    -- a Monoid instance for w.
    go :: Int -> Writer (Sum Int) ()
    go 1 = return ()
    go n = do
      W.tell 1
      if even n
        then go (n `div` 2)
        else go (3 * n + 1)

{-
Problem 79: (**) Postfix notation.

Postfix notation, also known as reverse Polish notation, has operators come after their operands
in mathematical expressions. It has no need for operator precedence or parentheses to specify
evaluation order.

Evaluation is typically done using a stack. Numbers are pushed onto the stack, and operators pop
out numbers and pushes back the result. The State monad would be useful for maintaining such a stack.

There may be errors with some expressions. For example, an expression may be ill-formed, or there
may be a division by zero. It would be useful to use the Maybe monad so that we can return Nothing
if there is an error.

Finally for this problem, we would like to keep track of changes to the stack and which operators
are applied to which numbers. The function should also return a list, with each entry showing the
state of the stack after an operand has been pushed or an operator has been applied.
Logging each entry can be done with the Writer monad.

Unfortunately, it would be very cumbersome to use these monads directly together. Monad transformers
are a way to make it substantially easier to use more than one monad at the same time.
Use monad transformers to compose the State, Maybe, and Writer monads into a single monad to
implement a function which evaluates an expression in postfix notation. It should also return the
history of the calculation.
-}

-- Encodes an operator for a mathematical expression.
data Operator
  = -- Encodes negation.  Equivalent to an unary minus.  Unary operator.
    Negate
  | -- Encodes duplication.  Makes another copy of its operand.  Unary operator.
    Add
  | -- Encodes subtraction.  Binary operator.
    Subtract
  | -- Encodes multiplication.  Binary operator.
    Multiply
  | -- Encodes division.  Equivalent to 'div'.  Binary operator.
    Divide
  | -- Encodes a modulo operator.  Equivalent to 'mod'.  Binary operator.
    Modulo
  deriving stock (Show, Eq)

-- A single element within a mathematical expression.
-- A list of these elements comprises an expression in postfix notation.
data Element = Operator Operator | Operand Integer deriving stock (Show, Eq)

type Stack = [Integer]

type Logger = Writer [(Stack, Maybe Operator)]

type Result = Maybe Integer

calculatePostfix :: [Element] -> (Result, [(Stack, Maybe Operator)])
calculatePostfix xs = (result, calculations)
  where
    ((result, _), calculations) = Id.runIdentity $ W.runWriterT $ S.runStateT (calc xs) []

calc :: [Element] -> StateT Stack Logger Result
calc [] = S.gets extract
calc (Operand n : xs) = S.get >>= loop xs Nothing . (n :)
calc (Operator op : xs) =
  S.get >>= Mb.maybe (return Nothing) (loop xs (Just op)) . calc' op

extract :: [Integer] -> Maybe Integer
extract [x] = Just x
extract _ = Nothing

loop :: [Element] -> Maybe Operator -> Stack -> StateT Stack Logger Result
loop xs op s = do
  W.tell [(s, op)]
  S.put s
  calc xs

calc' :: Operator -> Stack -> Maybe Stack
calc' el s = case el of
  Negate -> unaryOp s
  op -> binaryOp op s

unaryOp :: Stack -> Maybe Stack
unaryOp [] = Nothing
unaryOp (x : xs) = Just (-x : xs)

binaryOp :: Operator -> Stack -> Maybe Stack
binaryOp op (y : x : xs) =
  (: xs) <$> case op of
    Add -> Just $ x + y
    Subtract -> Just $ x - y
    Multiply -> Just $ x * y
    Divide | y == 0 -> Nothing
    Divide -> Just $ x `div` y
    Modulo | y == 0 -> Nothing
    Modulo -> return $ x `mod` y
    _ -> Nothing
binaryOp _ _ = Nothing
