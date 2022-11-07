{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Miner
  ( initGame
  , minerMove
  , Game(..)
  , Direction(..)
  , dead, score, miner
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

minerMove :: Game -> Direction -> Game
minerMove gs0 dir
    | (gs0 ^. minerRestTick) /= 0 =
        let gsMinerRest = gs0 & minerRestTick .~ ((gs0 ^. minerRestTick) - 1)
        in gsMinerRest
    | Just nPos <- minerNewCoord gs0 dir =
        let gsMinerMove0 = gs0 & miner .~ nPos
            gsMinerMove1 = gsMinerMove0 & minerRestTick .~ (gs0 ^. minerRestTickConst)
        in gsMinerMove1
    | otherwise = gs0

minerNewCoord :: Game -> Direction -> Maybe Coord
minerNewCoord g dir
    | nPos <- neighborCoord dir (g ^. miner), coordInBound nPos
        = Just nPos
minerNewCoord _ _ = Nothing

coordInBound :: Coord -> Bool
coordInBound (V2 x y)
    | y < 0 = False
    | y >= height - 1 = False
    | x < 0 = False
    | x >= width - 1 = False
    | otherwise = True

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
        ym = height - 1
    in pure $ Game
        { _miner  = (V2 xm ym)
        , _minerRestTickConst = 2
        , _minerRestTick = 2
        , _score  = 0
        , _dead   = False
        }