module Monad.P79Spec (spec) where

import qualified Control.Monad as M
import qualified Data.Char as C
import Monad.P79
import Test.Hspec

parsePostfix :: String -> [Element]
parsePostfix = map parseToken . words

parseToken :: String -> Element
parseToken x
  | C.isDigit (head x) = Operand (read x)
  | x == "negate" = Operator Negate
  | x == "+" = Operator Add
  | x == "-" = Operator Subtract
  | x == "*" = Operator Multiply
  | x == "/" = Operator Divide
  | x == "%" = Operator Modulo
  | otherwise = error $ "unknown token: " ++ x

spec :: Spec
spec = do
  describe "calculatePostfix" $ do
    it "evaluates an expression in postfix notation" $ do
      let xs =
            [ ("8 5 4 10 + - 3 * negate +", Just 35),
              ("8 5 -", Just 3),
              ("8 6", Nothing),
              ("8 negate", Just (-8)),
              ("8 +", Nothing)
            ]
      M.forM_ xs $ \(ex, res) -> do
        let expr = parsePostfix ex
        let result = calculatePostfix expr
        fst result `shouldBe` res

    it "logs each step of a computation" $ do
      let expr = parsePostfix "8 5 4 10 + - 3 * negate +"
      let result = calculatePostfix expr

      snd result
        `shouldBe` [ ([8], Nothing),
                     ([5, 8], Nothing),
                     ([4, 5, 8], Nothing),
                     ([10, 4, 5, 8], Nothing),
                     ([14, 5, 8], Just Add),
                     ([-9, 8], Just Subtract),
                     ([3, -9, 8], Nothing),
                     ([-27, 8], Just Multiply),
                     ([27, 8], Just Negate),
                     ([35], Just Add)
                   ]

    it "logs each step of a failed computation" $ do
      let expr = parsePostfix "1 2 * +"
      let result = calculatePostfix expr

      snd result
        `shouldBe` [ ([1], Nothing),
                     ([2, 1], Nothing),
                     ([2], Just Multiply)
                   ]
