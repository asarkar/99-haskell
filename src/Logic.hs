{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Logic where

import qualified Control.Monad as M
import qualified Data.Bifunctor as Bf
import qualified Data.Bits as B
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as Q
import qualified Text.Printf as P

-- true if and only if both a and b are true.
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

-- true if and only if one or both of a and b are true.
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

-- true if and only if (and' a b) is false.
nand' :: Bool -> Bool -> Bool
nand' b = not . and' b

-- true if and only if (or' a b) is false.
nor' :: Bool -> Bool -> Bool
nor' b = not . or' b

-- true if and only if exactly one of a and b is true.
xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

-- b must be true if a is true.
-- If a is false, there is no implication as to what b should be.
impl' :: Bool -> Bool -> Bool
impl' True b = b
impl' False _ = True

-- a is true if and only if b is true.
equ' :: Bool -> Bool -> Bool
equ' a True = a
equ' a False = not a

{-
Problem 46: (**) Truth tables for logical expressions.

Write a predicate table/3 which prints the truth table
of a given logical expression in two variables.
-}
table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = [(a, b, f a b) | a <- [True, False], b <- [True, False]]

{-
Problem 47: (*) Truth tables for logical expressions (part 2).

Continue Problem 46 by defining and/2, or/2, etc as being operators.
This allows to write the logical expression in the more natural way.

ANSWER: Any operator can be used as infix in Haskell. If it is
alphanumeric, then backticks are needed.
-}

{-
Problem 48: (*) Truth tables for logical expressions (part 3).

Generalize Problem 47 in such a way that the logical expression
may contain any number of logical variables.
-}
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = map (M.ap (++) (pure . f)) $ M.replicateM n [True, False]

{-
Problem 49: (**) Gray codes.

Given a number of bits n, generate a gray code for it.

For example, for n = 2, one gray code would be [00, 01, 11, 10].

For number of bits n, there are 2^n Gray codes, including zero.
Thus, the maximum Gray code is 2^n - 1. Since, by definition,
Gray codes differ only by 1 bit from their neighbors, the i-th
Gray code is given by the XOR of the i-th and the (i - 1)th bits
of the binary representation of i, where 0 <= i < 2^n.

For the example above, n = 2, the corresponding Gray codes are:
0 ^ 0 = 0 (00), 1 ^ 0 = 1 (01), 2 ^ 1 = 3 (11) and, 3 ^ 1 = 2 (10)
-}
gray :: Int -> [String]
gray n = [P.printf "%0*b" n (g i) | i <- [0 .. x - 1]]
  where
    x = 1 `B.shiftL` n :: Int -- 2 ^ n - 1
    g i = i `B.xor` (i `B.shiftR` 1) -- i xor (i `div` 2)

{-
Problem 50: (***) Given a list of characters and their number of occurrences,
construct a list of the characters and their Huffman encoding.

ANSWER:

https://brilliant.org/wiki/huffman-encoding/

The Huffman coding algorithm takes in information about the frequencies or probabilities
of a particular symbol occurring. It begins to build the prefix tree from the bottom up,
starting with the two least probable symbols in the list. It takes those symbols and
forms a subtree containing them, and then removes the individual symbols from the list.
The algorithm sums the probabilities of elements in a subtree and adds the subtree and
its probability to the list. Next, the algorithm searches the list and selects the two
symbols or subtrees with the smallest probabilities. It uses those to make a new subtree,
removes the original subtrees/symbols from the list, and then adds the new subtree and
its combined probability to the list. This repeats until there is one tree and all
elements have been added.
-}

data HTree = Branch HTree HTree | Leaf Char
  deriving stock (Eq, Show)

type PQ = HashPSQ String Int HTree

huffman :: [(Char, Int)] -> [(Char, String)]
huffman freq = treeToList $ go $ Q.fromList initial
  where
    initial = map (\(c, n) -> ([c], n, Leaf c)) freq

    minN :: Int -> PQ -> ([(String, Int, HTree)], PQ)
    minN 0 q = ([], q)
    minN n q = case Q.minView q of
      Nothing -> ([], q)
      Just (k, p, v, q') ->
        let (xs, q'') = minN (n - 1) q'
         in ((k, p, v) : xs, q'')

    go :: PQ -> HTree
    go q = case Q.size q of
      0 -> error "empty heap"
      1 -> let ((_, _, tree) : _, _) = minN 1 q in tree
      _ -> do
        let ((k1, p1, v1) : (k2, p2, v2) : _, q') = minN 2 q
        let node =
              if p1 < p2
                then Branch v1 v2
                else Branch v2 v1
        go $ Q.insert (k1 ++ k2) (p1 + p2) node q'

    {-
    Although Huffman coding doesn't specify which of the
    two values to put on which side (i.e., left or right),
    the examples put the smaller value on the left.
    Also, the examples encode the left branch as 0.
    -}
    treeToList :: HTree -> [(Char, String)]
    treeToList (Leaf x) = [(x, "")]
    treeToList (Branch l r) = left ++ right
      where
        left = map (Bf.second ('0' :)) $ treeToList l
        right = map (Bf.second ('1' :)) $ treeToList r

{-
Problem 51: (*) Error correction codes.

corrupt :: RandomGen g => g -> Int -> [Bool] -> [Bool]

Flip a given number of boolean values in the boolean list randomly.

Examples:
  >>> corrupt (mkStdGen 111) 2 [False, True, True, False, True]
  [False,False,True,True,False]

errorCorrectingEncode :: [Bool] -> [Bool]

Construct an error-correcting encoding of the given Boolean list.

The encoding must be able to correct at least one error.
Consider using a repetition code of length 3.

errorCorrectingDecode :: [Bool] -> [Bool]

The inverse of errorCorrectingEncode. Recover the original Boolean list from its encoding.
There could be a single error in the encoding.

Examples:

  >>> errorCorrectingDecode . errorCorrectingEncode $ [False, False, True, False]
  [False,False,True,False]
  ---
  >>> let e = errorCorrectingEncode [True, False, False, True, False]
  >>> let e' = corrupt (mkStdGen 111) 1 e
  >>> errorCorrectingDecode e'
  [True,False,False,True,False]

ANSWER: TODO.
-}

{-
Problem 52: (***) Conjunctive normal form.

It is known that any boolean function can be represented in conjunctive normal form.
These are conjunctions of disjunctions of literals, where literals are one of boolean
values, variables, or the complement of values or variables.

Return the conjunctive normal form of a boolean formula. The value returned should
always be a conjunction of disjunctions.

data Formula
  Constructors:

  Value Bool: A constant value.

  Variable String: A variable with given name.

  Complement Formula: Logical complement. I.e., it is true only if its clause is false.

  Disjoin [Formula]: Disjunction. I.e., it is true if any of its clauses are true.

  Conjoin [Formula]: toConjunctiveNormalForm :: Formula -> Formula

Examples:
  >>> toConjunctiveNormalForm $ Value True
  Conjoin [Disjoin [Value True]]

  >>> toConjunctiveNormalForm $ Complement $ Disjoin [Variable "X", Variable "Y"]
  Conjoin [Disjoin [Complement (Variable "X")],Disjoin [Complement (Variable "Y")]]

ANSWER: TODO.
-}

{-
Problem 53: (***) Resolution rule.

The problem description is a page long, don't even bother.
-}
