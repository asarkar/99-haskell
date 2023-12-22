module GenLists where

import qualified Control.Monad as M
import Test.QuickCheck

genNum :: Gen Int
genNum = frequency [(1, chooseInt (-100, -1)), (4, chooseInt (0, 100))]

genList :: Gen a -> Gen [a]
genList ga = chooseInt (0, 100) >>= (`vectorOf` ga)

genNEList :: Gen a -> Gen [a]
genNEList ga = chooseInt (1, 100) >>= (`vectorOf` ga)

genListAndLen :: Gen a -> Gen ([a], Int)
genListAndLen g = genList g >>= andLength

genNEListAndLen :: Gen a -> Gen ([a], Int)
genNEListAndLen g = genNEList g >>= andLength

-- Given a list, returns a 2-tuple whose first element is the
-- same list, and the second element is an integer in the
-- range [1, n], where n is the length of the list.
--
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- When the m a1 and m a2 are functions, liftM2 applies the argument
-- to both and then combines the results.
-- See https://www.reddit.com/r/haskell/comments/18nxish/comment/keduusx/
--
-- Also see:
-- ap :: Monad m => m (a -> b) -> m a -> m b
-- https://stackoverflow.com/q/33454650/839733
andLength :: [a] -> Gen ([a], Int)
andLength = M.liftM2 (<$>) (,) (chooseInt . (1,) . length)

genRepeatedElem :: Gen a -> Gen [a]
genRepeatedElem g = (M.join <$>) . go =<< chooseInt (0, 50)
  where
    go size
      | size <= 0 = pure []
      | otherwise = do
          n <- frequency [(1, chooseInt (2, 5)), (3, pure 1)]
          x <- g
          let y = min size n
          xs <- go (size - y)
          return $ replicate y x : xs
