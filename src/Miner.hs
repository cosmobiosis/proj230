{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Miner
  ( initGame
  , gameTickAction
  , minerMove
  , minerPutBomb
  , Game(..)
  , Direction(..)
  , dead, score, miner, minerDirc, bomb, bombState
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
import System.Random (Random(..), newStdGen)

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

bombTickAction :: Game -> Game
bombTickAction gs0 = gs0 & bombTick .~ (gs0 ^. bombTick -1)

bombNextState :: Game -> Game
bombNextState gs0
  | gs0 ^. bombState == 2 = gs0 & bombState .~ (-1) & bombTick .~ (gs0 ^. bombTickConst)
  | otherwise = 
    gs0 & bombState .~ (gs0 ^. bombState + 1) & bombTick .~ (gs0 ^. bombTickConst)

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
            in gs1
        -- bomb entering next state
        | otherwise 
          = bombNextState gs0
  in bombg
  
minerMove :: Game -> Direction -> Game
minerMove gs0 dir
    | gs0 ^. minerDirc /= dir =
        let gsMinerTurn = gs0 & minerDirc .~ dir
        in gsMinerTurn
    | (gs0 ^. minerRestTick) /= 0 =
        let gsMinerRest = gs0 & minerRestTick .~ ((gs0 ^. minerRestTick) - 1)
        in gsMinerRest
    | Just nPos <- facingNewCoord gs0 dir =
            if  coordEmpty gs0 nPos
            then
                gs0 & 
                miner .~ nPos & 
                minerRestTick .~ (gs0 ^. minerRestTickConst)
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

coordEmpty :: Game -> Coord -> Bool
coordEmpty g c
    | c /= (g ^. miner) &&
      not (bombExist g c) &&
      notElem c (g ^. boulders) = True
    | otherwise = False

neighborCoord :: Direction -> Coord -> Coord
neighborCoord dir (V2 x y)
  | dir == North = V2 x (y+1)
  | dir == South = V2 x (y-1)
  | dir == East  = V2 (x+1) y
  | dir == West  = V2 (x-1) y
  | otherwise = V2 0 0

initGame :: IO Game
initGame =
    let xm = width `div` 2
        ym = 0
    in pure $ Game
        { _miner  = (V2 xm ym)
        , _minerRestTickConst = 2
        , _minerRestTick = 2
        , _score  = 0
        , _dead   = False
        , _bomb = (V2 0 0)
        , _bombTickConst = 15
        , _bombTick = 15
        , _bombState = -1
        , _boulders = []
        , _minerDirc = North
        }