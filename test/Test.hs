{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Prelude hiding (maximum)
import Common
import MyUtils

main :: IO ()
main = runTests 
  [ testLastN
  ]

testLastN ::  Score -> TestTree
testLastN sc = testGroup "lastN" [
  scoreTest ((\_ -> lastN 3 ["a", "b", "c", "x", "y", "z"]), (), ["x", "y", "z"], 1, "lastn-1")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

-- prop_lastN :: Int -> [a] -> Bool
-- prop_lastN n xs = length (lastN n xs) <= n