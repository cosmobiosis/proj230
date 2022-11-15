{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Miner
  ( initGame
  , gameTickAction
  , minerMove
  , minerPutBomb
  , inBlastRange
  , Game(..)
  , Direction(..)
  , dead, score, miner, minerDirc, bomb, bombState, blastCrossRadius, earths, boulders
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen, randomIO, randomRIO)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import System.Random.Stateful (StdGen)
import System.Random (mkStdGen)

data Game = Game
  { _miner  :: Coord        -- ^ miner as a point of coord
  , _score  :: Int          -- ^ score
  , _minerDirc :: Direction
  , _boulders :: [Coord]
  , _bomb :: Coord
  , _bombTickConst :: Int
  , _bombTick :: Int
  , _bombState :: Int -- -1 means no bomb been placed, 0 means bomb0, 1 means bomb1, 2 means bomb exploding
  , _dead   :: Bool
  , _minerRestTickConst :: Int
  , _minerRestTick :: Int
  , _blastCrossRadius :: Int
  , _blastSquareRadius :: Int
  , _earths :: [Coord]
  } deriving (Show)

type Coord = V2 Int
type Miner = Coord

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 40
width = 40

-- Utils
bombTickAction :: Game -> Game
bombTickAction gs0 = gs0 & bombTick .~ (gs0 ^. bombTick -1)

bombNextState :: Game -> Game
bombNextState gs0
  | gs0 ^. bombState == 2 = gs0 & bombState .~ (-1) & bombTick .~ (gs0 ^. bombTickConst)
  | otherwise =
    gs0 & bombState .~ (gs0 ^. bombState + 1) & bombTick .~ (gs0 ^. bombTickConst)

inBlastRange :: Game -> Coord -> Bool
inBlastRange g (V2 x y)
  | g ^. bomb ^. _x == x && abs ((g ^. bomb ^. _y) - y) <= g ^. blastCrossRadius = True
  | g ^. bomb ^. _y == y && abs ((g ^. bomb ^. _x) - x) <= g ^. blastCrossRadius = True
  | (abs ((g ^. bomb ^. _x) - x) <= g ^. blastSquareRadius) && (abs ((g ^. bomb ^. _y) - y) <= g ^. blastSquareRadius) = True
  | otherwise = False

gameTickAction :: Game -> Game
gameTickAction gs0 =
  let bombg
        -- bomb not setting up
        | gs0 ^. bombState == -1
          = gs0
        -- bomb ticking
        | (gs0 ^. bombTick /= 0)
          = bombTickAction gs0
        -- bomb begin exploding
        | (gs0 ^. bombState == 1)
          = let gs1 = bombNextState gs0
                gs2 = gs1 & earths .~ filter (not . inBlastRange gs0) (gs0 ^. earths)
                gs3 = gs2 & boulders .~ filter (not . inBlastRange gs0) (gs0 ^. boulders)
            in gs3
        -- bomb entering next state
        | otherwise
          = bombNextState gs0
  in bombg

delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]

minerMove :: Game -> Direction -> Game
minerMove gs0 dir
    | gs0 ^. minerDirc /= dir =
        let gsMinerTurn = gs0 & minerDirc .~ dir
        in gsMinerTurn
    | (gs0 ^. minerRestTick) /= 0 =
        let gsMinerRest = gs0 & minerRestTick .~ ((gs0 ^. minerRestTick) - 1)
        in gsMinerRest
    | Just nPos <- facingNewCoord gs0 dir =
            if coordIsEarth gs0 nPos || coordEmpty gs0 nPos
            then
                gs0 &
                miner .~ nPos &
                minerRestTick .~ (gs0 ^. minerRestTickConst) &
                earths .~ delete nPos (gs0 ^. earths)
                -- let gsMinerMove0 = gs0 & miner .~ nPos
                --     gsMinerMove1 = gsMinerMove0 & minerRestTick .~ (gs0 ^. minerRestTickConst)
                -- in gsMinerMove1
            else
              gs0
    | otherwise = gs0

bombExist :: Game -> Coord -> Bool
bombExist gs0 targetCoord
  | (gs0 ^. bombState) /= (-1) && targetCoord == gs0 ^. bomb = True
  | otherwise = False

minerPutBomb :: Game -> Game
minerPutBomb gs0
    | (gs0 ^. bombState) /= (-1) = gs0
    | Just nPos <- facingNewCoord gs0 (gs0 ^. minerDirc) =
        if coordEmpty gs0 nPos
          then gs0 & bomb .~ nPos & bombState .~ 0
        else
          gs0
    | otherwise = gs0

facingNewCoord :: Game -> Direction -> Maybe Coord
facingNewCoord g dir
    | nPos <- neighborCoord dir (g ^. miner), coordInBound nPos
        = Just nPos
facingNewCoord _ _ = Nothing

coordInBound :: Coord -> Bool
coordInBound (V2 x y)
    | y < 0 = False
    | y >= height - 1 = False
    | x < 0 = False
    | x >= width - 1 = False
    | otherwise = True

coordIsEarth :: Game -> Coord -> Bool
coordIsEarth g c
  | c `elem` g ^. earths = True
  | otherwise = False

coordEmpty :: Game -> Coord -> Bool
coordEmpty g c
    | c /= (g ^. miner) &&
      not (bombExist g c) &&
      notElem c (g ^. boulders) &&
      notElem c (g ^. earths) = True
    | otherwise = False

neighborCoord :: Direction -> Coord -> Coord
neighborCoord dir (V2 x y)
  | dir == North = V2 x (y+1)
  | dir == South = V2 x (y-1)
  | dir == East  = V2 (x+1) y
  | dir == West  = V2 (x-1) y
  | otherwise = V2 0 0

padEarthRow :: Int -> Int -> [Coord]
padEarthRow x y
  | x == width = []
  | x == width `div` 2 = padEarthRow (x+1) y
  | otherwise = V2 x y : padEarthRow (x+1) y

padEarth :: Int -> [Coord]
padEarth y
  | y == height - 1 = []
  | otherwise = padEarthRow 0 y ++ padEarth (y + 1)

initEarth :: [Coord]
initEarth = padEarth 0

padBoulderRow :: Int -> Int -> [Coord]
padBoulderRow x y
  | x == width = []
  | (x + y == width || abs (x - y) == 7) && (x /= (width `div` 2)) = V2 x y : padBoulderRow (x+1) y
  | otherwise = padBoulderRow (x+1) y

padBoulder :: Int -> [Coord]
padBoulder y
  | y == height - 1 = []
  | otherwise = padBoulderRow 0 y ++ padBoulder (y + 1)

initBoulder :: [Coord]
initBoulder = padBoulder 0

initGame :: IO Game
initGame =
    let xm = width `div` 2
        ym = 0
        initialBoulders = initBoulder
        initialEarths = filter (`notElem` initialBoulders) initEarth
    in pure $ Game
        { _miner  = V2 xm ym
        , _minerRestTickConst = 2
        , _minerRestTick = 2
        , _score  = 0
        , _dead   = False
        , _bomb = V2 0 0
        , _earths = initialEarths
        , _boulders = initialBoulders
        , _bombTickConst = 12
        , _bombTick = 15
        , _bombState = -1
        , _minerDirc = North
        , _blastCrossRadius = 1
        , _blastSquareRadius = 0
        }