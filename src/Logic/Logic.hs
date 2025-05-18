module Logic.Logic where

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
