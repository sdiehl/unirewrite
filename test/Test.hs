module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Mini
import Match

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

test1 :: Assertion
test1 = matchList p x @?= m
  where
    p = List [Var "x", List [Int 5]]
    x = List [Int 3, List [Int 5]]
    m = [(Var "x", Int 3)]

test2 :: Assertion
test2 = matchList p x @?= m
  where
    p = List [Var "x", Var "y"]
    x = List [Int 3, List [Int 5]]
    m = [(Var "x", Int 3), (Var "y", List [Int 5])]

test3 :: Assertion
test3 = matchList p x @?= m
  where
    p = List [Int 3, Var "y"]
    x = List [Int 3, List [Int 5]]
    m = [(Var "y", List [Int 5])]

test4 :: Assertion
test4 = matchList p x @?= m
  where
    p = List [List [Var "x"], List [Var "y"]]
    x = List [List [Int 1], List [Int 2]]
    m = [(Var "x", Int 1), (Var "y", Int 2)]

test5 :: Assertion
test5 = assert $ not $ matches p x
  where
    p = List [List [Int 3], List [Var "y"]]
    x = List [List [Int 1], List [Int 2]]

test6 :: Assertion
test6 = assert $ not $ matches p x
  where
    p = Int 3
    x = Int 1

test7 :: Assertion
test7 = matchList p x @?= m
  where
    p = Var "x"
    x = Int 1
    m = [(Var "x", Int 1)]

unitTests :: TestTree
unitTests = testGroup "Pipeline tests"
  [
    testCase "testPattern1" $ test1
  , testCase "testPattern2" $ test2
  , testCase "testPattern3" $ test3
  , testCase "testPattern4" $ test4
  , testCase "testPattern5" $ test5
  , testCase "testPattern6" $ test6
  , testCase "testPattern7" $ test7
  ]
