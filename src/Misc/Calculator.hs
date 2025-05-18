{-# OPTIONS -Wno-incomplete-patterns #-}

module Misc.Calculator (eval) where

import qualified Data.Char as C

parseInt :: Int -> String -> (Int, String)
parseInt s [] = (s, "")
parseInt s ys@(x : xs)
  | C.isDigit x = parseInt (s * 10 + C.digitToInt x) xs
  | otherwise = (s, ys)

handleOp :: Char -> [Rational] -> String -> ([Rational], String)
handleOp _ numStack [] = (numStack, [])
handleOp x numStack opStack@(op : _)
  | op `elem` "*/" || x `elem` "+-" =
      let (ns, os) = evalStacks numStack opStack
       in handleOp x ns os
  | otherwise = (numStack, opStack)

evalStacks :: [Rational] -> String -> ([Rational], String)
evalStacks (y : x : rest) (op : ops) = (calculate x y op : rest, ops)
evalStacks [x] opStack
  | null opStack || head opStack == '+' = ([x], "")
  | otherwise = ([-x], "")

calculate :: Rational -> Rational -> Char -> Rational
calculate x y op = case op of
  '+' -> x + y
  '-' -> x - y
  '*' -> x * y
  '/' -> x / y

eval' :: [Rational] -> String -> String -> (Rational, String)
eval' [x] [] [] = (x, "")
eval' numStack opStack [] =
  let (ns, os) = evalStacks numStack opStack
   in eval' ns os []
eval' numStack opStack ys@(x : xs)
  | x == '(' =
      let (i, zs) = eval' [] [] xs
       in eval' (i : numStack) opStack zs
  | x == ')' =
      let (i, _) = eval' numStack opStack []
       in (i, xs)
  | C.isSpace x = eval' numStack opStack xs
  | C.isDigit x =
      let (i, rest) = parseInt 0 ys
       in eval' ((fromIntegral i :: Rational) : numStack) opStack rest
  | otherwise =
      let (ns, os) = handleOp x numStack opStack
       in eval' ns (x : os) xs

-- This is basically LeetCode 224: Basic Calculator
-- https://leetcode.com/problems/basic-calculator/description/
-- It was written for P93, but currently unused.
eval :: String -> Rational
eval = fst . eval' [] []
