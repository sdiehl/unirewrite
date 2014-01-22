module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Mini
import Match
import Pattern

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [matchTests, patternTests]

-------------------------------------------------------------------------------
-- Match Tests
-------------------------------------------------------------------------------

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

test8 :: Assertion
test8 = matchList p x @?= m
  where
    p = Var "x"
    x = List [Int 3, List [Int 5]]
    m = [(Var "x", List [Int 3, List [Int 5]])]

matchTests :: TestTree
matchTests = testGroup "Match Tests"
  [
    testCase "testPattern1" $ test1
  , testCase "testPattern2" $ test2
  , testCase "testPattern3" $ test3
  , testCase "testPattern4" $ test4
  , testCase "testPattern5" $ test5
  , testCase "testPattern6" $ test6
  , testCase "testPattern7" $ test7
  , testCase "testPattern8" $ test8
  ]

-------------------------------------------------------------------------------
-- Pattern Tests
-------------------------------------------------------------------------------

ptest1 :: Assertion
ptest1 = pattern (l, r) x @?= Just out
  where
    l   = Var "x"
    r   = Var "x"
    x   = Int 1
    out = Int 1

ptest2 :: Assertion
ptest2 = pattern (l, r) x @?= Nothing
  where
    l   = List [Var "x"]
    r   = List [Var "x", Int 2]
    x   = Int 1

ptest3 :: Assertion
ptest3 = pattern (l, r) x @?= Just out
  where
    l   = List [Var "x"]
    r   = List [Var "x", Int 2]
    x   = List [Int 1]
    out = List [Int 1, Int 2]

ptest4 :: Assertion
ptest4 = pattern (l, r) x @?= Just out
  where
    l   = Int 1
    r   = Int 2
    x   = Int 1
    out = Int 2

ptest5 :: Assertion
ptest5 = gpattern (l, r, g) x @?= Just out
  where
    l   = Int 1
    r   = Int 2
    g   = Bool True
    x   = Int 1
    out = Int 2

ptest6 :: Assertion
ptest6 = gpattern (l, r, g) x @?= Nothing
  where
    l   = Int 1
    r   = Int 2
    g   = Bool False
    x   = Int 1

ptest7 :: Assertion
ptest7 = cases x cs @?= Just out
  where
    x  = Int 2
    cs = [
        (Int 1, Int 2)
      , (Int 2, Int 3)
      ]
    out = Int 3

ptest8 :: Assertion
ptest8 = gcases x cs @?= Just out
  where
    x  = Int 1
    cs = [
        (Int 1, Int 2, Bool False)
      , (Int 1, Int 3, Bool True)
      ]
    out = Int 3

ptest9 :: Assertion
ptest9 = gcases x cs @?= Just out
  where
    x  = Bool True
    cs = [
        (Var "x", Int 1, Var "x")
      ]
    out = Int 1

ptest10 :: Assertion
ptest10 = gcases x cs @?= Just out
  where
    x  = Bool False
    cs = [
        (Var "x", Int 1, false)
      , (Var "x", Int 2, true)
      ]
    out = Int 2

patternTests :: TestTree
patternTests = testGroup "Pattern Tests"
  [
    testCase "testPattern1" $ ptest1
  , testCase "testPattern2" $ ptest2
  , testCase "testPattern3" $ ptest3
  , testCase "testPattern4" $ ptest4
  , testCase "testPattern5" $ ptest5
  , testCase "testPattern6" $ ptest6
  , testCase "testPattern7" $ ptest7
  , testCase "testPattern8" $ ptest8
  , testCase "testPattern9" $ ptest9
  , testCase "testPattern10" $ ptest10
  ]
