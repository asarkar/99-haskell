module Misc.CalculatorSpec (spec) where

import qualified Control.Monad as M
import Misc.Calculator
import Test.Hspec
import Test.Hspec.Expectations.Contrib
import qualified Text.Printf as P

expectEq :: (HasCallStack, Eq a, Show a) => String -> a -> a -> Expectation
expectEq prefix actual expected = M.unless (actual == expected) (expectationFailure msg)
  where
    msg = P.printf "[%s] expected: %s, but got: %s" prefix (show expected) (show actual)

spec :: Spec
spec = do
  describe "calculator" $ do
    it "evaluates expression" $ do
      let xs =
            [ ("3+2*2", 7),
              (" 3/2 ", 1),
              (" 3+5 / 2 ", 5),
              ("42", 42),
              ("1-1+1", 1),
              ("1-1-1", -1),
              ("1*2-3/4+5*6-7*8+9/10", -23), -- -23.85
              ("1+2*5/3+6/4*2", 7), -- 7.33
              ("282-1*2*13-30-2*2*2/2-95/5*2+55+804+3024", 4067),
              ("(1+(4+5+2)-3)+(6+8)", 23),
              ("- (3 + (4 + 5))", -12),
              ("(6)-(8)-(7)+(1+(6))", -2),
              ("2-(5-6)", 3),
              ("-2+ 1", -1),
              ("1-(     -2)", 3),
              ("(1)", 1)
            ]

      M.forM_ xs $ \(expr, expected) -> do
        annotate expr $ do
          let actual = truncate (eval expr) :: Int
          actual `shouldBe` expected
