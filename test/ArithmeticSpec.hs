module ArithmeticSpec (spec) where

import Arithmetic
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

  describe "myGCD" $ do
    it "calculates the GCD of two positive integers" $ do
      myGCD 36 63 `shouldBe` 9
      myGCD 125 81 `shouldBe` 1
      myGCD 221 559 `shouldBe` 13

  describe "coprime" $ do
    it "determines whether two positive integers are coprime" $ do
      coprime 35 64 `shouldBe` True
      coprime 1173 1547 `shouldBe` False

  describe "totient" $ do
    it "calculates Euler's totient function phi" $ do
      totient 10 `shouldBe` 4

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

  describe "primeFactorsMult" $ do
    it "calculates the prime factors and their multiplicities of a positive integer" $ do
      primeFactorsMult 315 `shouldBe` [(3, 2), (5, 1), (7, 1)]

  describe "primesR" $ do
    it "constructs a list of primes within a given range" $ do
      primesR 10 20 `shouldBe` [11, 13, 17, 19]
      primesR 7 31 `shouldBe` [7, 11, 13, 17, 19, 23, 29, 31]

  describe "goldbach" $ do
    it "finds two prime numbers that sum to a given even integer" $ do
      goldbach 28 `shouldBe` (5, 23)

  describe "goldbachList" $ do
    it "finds Goldbach composition of all erven numbers within a given range" $ do
      goldbachList 9 20 `shouldMatchList` [(3, 7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)]

  describe "multiplicativeInverse" $ do
    it "the multiplicative inverse of a given integer a and modulus b" $ do
      let xs = [(3 :: Int, 5, Just 2), (48, 127, Just 45), (824, 93, Just 50), (48, 93, Nothing)]
      M.forM_ xs $ \(a, b, mmi) ->
        multiplicativeInverse a b `shouldBe` mmi
