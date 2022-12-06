{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Prelude hiding (maximum)
import Common
import MyUtils

main :: IO ()
main = runTests 
  [ testLastN,
    testCircInc,
    testSwapElems
  ]

testLastN ::  Score -> TestTree
testLastN sc = testGroup "lastN" [
  scoreTest ((\_ -> lastN 3 ["a", "b", "c", "x", "y", "z"]), (), ["x", "y", "z"], 1, "lastn-1"),
  scoreTest ((\_ -> lastN 3 ["y", "z"]), (), ["y", "z"], 1, "lastn-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

testCircInc ::  Score -> TestTree
testCircInc sc = testGroup "circ inc" [
  scoreTest ((\_ -> circInc 3 5 1), (), 4, 1, "circinc-1"),
  scoreTest ((\_ -> circInc 5 5 1), (), 0, 1, "circinc-2"),
  scoreTest ((\_ -> circInc 0 5 (-1)), (), 5, 1, "circinc-3"),
  scoreTest ((\_ -> circInc 5 5 (-1)), (), 4, 1, "circinc-4")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

testSwapElems ::  Score -> TestTree
testSwapElems sc = testGroup "swap" [
  scoreTest ((\_ -> swapElems (0, 1) ["a", "b"]), (), ["b", "a"], 1, "swap-1"),
  scoreTest ((\_ -> swapElems (1, 3) ["a", "x", "b", "y"]), (), ["a", "y", "b", "x"], 1, "swap-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
