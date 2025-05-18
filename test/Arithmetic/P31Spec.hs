module Arithmetic.P31Spec (spec) where

import Arithmetic.P31
import qualified Control.Monad as M
import Test.Hspec

spec :: Spec
spec = do
  describe "isPrime" $ do
    it "determines if a given integer is prime" $ do
      let xs =
            [ (1, False),
              (2, True),
              (3, True),
              (4, False),
              (5, True),
              (6, False),
              (7, True),
              (11, True)
            ]
      M.forM_ xs $ \(n, prime) ->
        isPrime n `shouldBe` prime
