{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module MyUtils
  ( 
    knuthShuffle,
    lastN,
    circInc,
    swapElems
  ) where

import Data.List
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen, randomIO, randomRIO)

import System.Random.Stateful (StdGen)
import System.Random (mkStdGen)


mkRands = mapM (randomRIO.(,)0 ). enumFromTo 1. pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l = let (a,b) = splitAt i l in a++c:drop 1 b

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i,j) xs | i==j = xs
                   | otherwise = replaceAt j (xs!!i) $ replaceAt i (xs!!j) xs

knuthShuffle :: [a] -> IO [a]
knuthShuffle xs =
  fmap (foldr swapElems xs. zip [1..]) (mkRands (length xs))

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

circInc :: Int -> Int -> Int -> Int
circInc n upper inc
  | n + inc > upper = 0
  | n + inc < 0 = upper
  | otherwise = n + inc