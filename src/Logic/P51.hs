module Logic.P51 where

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
