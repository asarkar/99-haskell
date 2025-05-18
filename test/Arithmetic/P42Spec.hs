module Arithmetic.P42Spec (spec) where

import Arithmetic.P42
import qualified Control.Monad as M
import Test.Hspec

spec :: Spec
spec = do
  describe "multiplicativeInverse" $ do
    it "the multiplicative inverse of a given integer a and modulus b" $ do
      let xs = [(3 :: Int, 5, Just 2), (48, 127, Just 45), (824, 93, Just 50), (48, 93, Nothing)]
      M.forM_ xs $ \(a, b, mmi) ->
        multiplicativeInverse a b `shouldBe` mmi
