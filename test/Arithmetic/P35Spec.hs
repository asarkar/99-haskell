module Arithmetic.P35Spec (spec) where

import Arithmetic.P35
import qualified Control.Monad as M
import Test.Hspec

spec :: Spec
spec = do
  describe "primeFactors" $ do
    it "calculates the prime factors of a positive integer" $ do
      let xs =
            [ (1, []),
              (2, [2]),
              (9, [3, 3]),
              (8, [2, 2, 2]),
              (12, [2, 2, 3]),
              (315, [3, 3, 5, 7]),
              (901255, [5, 17, 23, 461]),
              (93819012551, [11, 9539, 894119])
            ]
      M.forM_ xs $ \(n, primes) ->
        primeFactors n `shouldBe` primes
