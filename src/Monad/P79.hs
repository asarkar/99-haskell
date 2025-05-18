{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Monad.P79 (calculatePostfix, Operator (..), Element (..)) where

import qualified Control.Monad as M
import Control.Monad.State (MonadState, State)
import qualified Control.Monad.State as S
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as Mb
import Control.Monad.Writer.Strict (MonadWriter, WriterT)
import qualified Control.Monad.Writer.Strict as W

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

type Logs = [(Stack, Maybe Operator)]

type Result = Maybe Integer

{-
Stack order matters. The output is in the reverse order,
i.e. the innermost monad result wraps the others.
((a, w), s), where a is Result, w is Logs, and s is Stack.

This implementation uses the "tagless final" approach:
- Functions are declared using type constraints instead of
  concrete types.
- Instantiation of a polymorphic function to a concrete type
  (aka interpreter) happens "at the end":
-}
type Calculation = MaybeT (WriterT Logs (State Stack)) Integer

calculatePostfix :: [Element] -> (Result, Logs)
calculatePostfix = fst . chain . calc
  where
    chain = flip S.runState [] . W.runWriterT . Mb.runMaybeT

calc ::
  (MonadWriter Logs m, MonadState Stack m, MonadFail m) =>
  [Element] ->
  m Integer
calc elems =
  S.get >>= case elems of
    [] -> result
    (Operand n : xs) -> loop xs Nothing . (n :)
    (Operator op : xs) -> runOp op M.>=> loop xs (Just op)

loop ::
  (MonadWriter Logs m, MonadState Stack m, MonadFail m) =>
  [Element] ->
  Maybe Operator ->
  Stack ->
  m Integer
loop xs op s = W.tell [(s, op)] >> S.put s >> calc xs

{-
The 'fail' invocations are using the MonadFail instance for Maybe,
(which is the monad 'm' in the function signatures below).
The error message is ignored by the instance, so, we don't
bother passing one.
-}
result :: (MonadFail m) => Stack -> m Integer
result [x] = return x
result _ = fail ""

runOp :: (MonadFail m) => Operator -> Stack -> m Stack
runOp Negate = unaryOp
runOp op = binaryOp op

unaryOp :: (MonadFail m) => Stack -> m Stack
unaryOp [] = fail ""
unaryOp (x : xs) = return (-x : xs)

binaryOp :: (MonadFail m) => Operator -> Stack -> m Stack
binaryOp op (y : x : xs) =
  (: xs) <$> case op of
    Add -> return $ x + y
    Subtract -> return $ x - y
    Multiply -> return $ x * y
    Divide | y == 0 -> fail ""
    Divide -> return $ x `div` y
    Modulo | y == 0 -> fail ""
    Modulo -> return $ x `mod` y
    _ -> fail ""
binaryOp _ _ = fail ""
