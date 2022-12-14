{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module Miner
  ( initGame
  , gameTickAction
  , minerTickAction
  , minerPutBomb
  , minerChangeBomb
  , inBlastRange
  , monster
  , monsterHP
  , Game(..)
  , Direction(..)
  , win, dead, score, miner, minerDirc, bomb, bombState, blastCrossRadius, earths, boulders, bombType
  , height, width
  ) where

import MyUtils
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe, isJust)

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
import Data.List (sortBy)

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
  , _win    :: Bool
  , _monsterHP :: Int
  , _minerRestTickConst :: Int
  , _minerRestTick :: Int
  , _monsterRestTickConst :: Int
  , _monsterRestTick :: Int
  , _blastCrossRadius :: Int
  , _blastSquareRadius :: Int
  , _blastCircleRadius :: Int
  , _earths :: [Coord]
  , _monster :: Coord
  , _bombType :: Int
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
height = 15
width = 15

-- Utils
bombTickAction :: Game -> Game
bombTickAction gs0 = gs0 & bombTick .~ (gs0 ^. bombTick -1)

bombNextState :: Game -> Game
bombNextState gs0
  | gs0 ^. bombState == 2 = gs0 & bombState .~ (-1) & bombTick .~ (gs0 ^. bombTickConst)
  | otherwise =
    gs0 & bombState .~ (gs0 ^. bombState + 1) & bombTick .~ (gs0 ^. bombTickConst)

bombX :: Game -> Int
bombX g = g ^. bomb ^. _x

bombY :: Game -> Int
bombY g = g ^. bomb ^. _y

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1 , y1) (x2 , y2) = round (sqrt (x'*x' + y'*y'))
    where
      x' = fromIntegral (x1 - x2)
      y' = fromIntegral (y1 - y2)

inBlastRange :: Game -> Coord -> Bool
inBlastRange g (V2 x y)
  | bombX g == x && abs (bombY g - y) <= g ^. blastCrossRadius = True
  | bombY g == y && abs (bombX g - x) <= g ^. blastCrossRadius = True
  | (abs (bombX g - x) <= g ^. blastSquareRadius) && (abs (bombY g - y) <= g ^. blastSquareRadius) = True
  | distance (bombX g, bombY g) (x, y) == g ^. blastCircleRadius = True
  | otherwise = False

setBombConfig :: Game -> Int -> Int -> Int -> Game
setBombConfig gs0 newCross newSquare newCircle =
  let gs1 = gs0 & blastCrossRadius .~ newCross
      gs2 = gs1 & blastSquareRadius .~ newSquare
      gs3 = gs2 & blastCircleRadius .~ newCircle
  in  gs3

minerChangeBomb :: Game -> Int -> Game
minerChangeBomb gs0 inc
  | gs0 ^. bombState /= -1 = gs0
  | otherwise = 
    let gs1 = gs0 & bombType .~ circInc (gs0 ^. bombType) 3 inc
        gs2 = setBombType gs1
    in gs2

setBombType :: Game -> Game
setBombType gs0
  | (gs0 ^. bombType) == 0 = setBombConfig gs0 2 0 0
  | (gs0 ^. bombType) == 1 = setBombConfig gs0 0 1 0
  | (gs0 ^. bombType) == 2 = setBombConfig gs0 0 0 2
  | otherwise = setBombConfig gs0 4 1 6

scoreTickAction :: Game -> Game
scoreTickAction gs0 =
  let numBouldersInRange = length (filter (inBlastRange gs0) (gs0 ^. boulders))
      monsterHit = inBlastRange gs0 (gs0 ^. monster)
      deltaScore = numBouldersInRange + (\x -> if x then 10 else 0) monsterHit
      gs1 = gs0 & score .~ (gs0 ^. score + deltaScore)
      gs2 = gs1 & monsterHP .~ (gs1 ^. monsterHP - (\x -> if x then 1 else 0) monsterHit)
  in  gs2

gameTickAction :: Game -> Game
gameTickAction gs0
  | gs0 ^. dead || gs0 ^. win = gs0
  | inBlastRange gs0 (gs0 ^. miner) && gs0 ^. bombState == 2 = gs0 & dead .~ True
  | otherwise =
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
                  gs2 = gs1 & earths .~ filter (not . inBlastRange gs1) (gs1 ^. earths)
                  gs3 = scoreTickAction gs2
                  gs4 = gs3 & boulders .~ filter (not . inBlastRange gs3) (gs3 ^. boulders)
              in gs4
          | (gs0 ^. bombState == 2)
          -- bomb exploding
            = let gs1 = bombNextState gs0
                  gs2 = scoreTickAction gs1
              in gs2
          -- bomb entering next state
          | otherwise
            = bombNextState gs0
        monsg = monsterTickAction bombg
    in monsg

delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]

monsterTickAction :: Game -> Game
monsterTickAction gs0
  | gs0 ^. monster == gs0 ^. miner =
      let gsDead = gs0 & dead .~ True
      in gsDead
  | gs0 ^. monsterHP <= 0 =
      let gsWin = gs0 & win .~ True
      in gsWin
  | (gs0 ^. monsterRestTick) /= 0 =
      let gsMonsterRest = gs0 & monsterRestTick .~ ((gs0 ^. monsterRestTick) - 1)
      in gsMonsterRest
  | otherwise =
      let dfsRes = monsterNextDir gs0
          nPos = case dfsRes of
            Just dir -> neighborCoord dir (gs0 ^. monster)
            Nothing -> gs0 ^. monster
          gs1 = gs0 & monster .~ nPos & monsterRestTick .~ (gs0 ^. monsterRestTickConst)
      in gs1

monsterNextDir :: Game -> Maybe Direction
monsterNextDir g =
  let c@(V2 x y) = g ^. monster
      minerC@(V2 xm ym) = g ^. miner
      visited = [c]
      r0 = monsterNextDirHelper g (V2 x (y + 1)) visited North 0
      r1 = monsterNextDirHelper g (V2 x (y - 1)) visited South 0
      r2 = monsterNextDirHelper g (V2 (x - 1) y) visited West 0
      r3 = monsterNextDirHelper g (V2 (x + 1) y) visited East 0
      [rr1, rr2, rr3, rr4] = sortBy (\ x y -> compare (snd x) (snd y)) [r0, r1, r2, r3]
  in fst rr1 <|> fst rr2 <|> fst rr3 <|> fst rr4

monsterNextDirHelper :: Game -> Coord -> [Coord] -> Direction -> Int -> (Maybe Direction, Int)
-- current: Coord
-- visited: [Coord]
-- initD: Direction
-- curDepth: Int
monsterNextDirHelper g c@(V2 x y) visited initD curDepth
  | c == g ^. miner = (Just initD, 0)
  | curDepth > width = (Nothing, 2147483647)
  | not (coordInBound c) = (Nothing, 2147483647)
  | not (coordEmpty g c) = (Nothing, 2147483647)
  | c `elem` visited = (Nothing, 2147483647)
  | isJust (fst rr1) = (Just initD, snd rr1 + 1)
  | isJust (fst rr2) = (Just initD, snd rr2 + 1)
  | isJust (fst rr3) = (Just initD, snd rr3 + 1)
  | isJust (fst rr4) = (Just initD, snd rr4 + 1)
  | otherwise = (Nothing, 2147483647)
  where
    res1 = monsterNextDirHelper g (V2 (x + 1) y) (visited ++ [c]) initD (curDepth + 1)
    res2 = monsterNextDirHelper g (V2 (x - 1) y) (visited ++ [c]) initD (curDepth + 1)
    res3 = monsterNextDirHelper g (V2 x (y + 1)) (visited ++ [c]) initD (curDepth + 1)
    res4 = monsterNextDirHelper g (V2 x (y - 1)) (visited ++ [c]) initD (curDepth + 1)
    [rr1, rr2, rr3, rr4] = sortBy (\ x y -> compare (snd x) (snd y)) [res1, res2, res3, res4]

minerTickAction :: Game -> Direction -> Game
minerTickAction gs0 dir
    | gs0 ^. dead || gs0 ^. win = gs0
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
    | y >= height = False
    | x < 0 = False
    | x >= width = False
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

takeCoords :: [Coord] -> Int -> ([Coord], [Coord])
takeCoords coords n = (take n coords, lastN (length coords - n) coords)

initGame :: IO Game
initGame = do
  allCoords <- knuthShuffle [V2 i j | i <- [0 .. width - 1], j <- [0 .. height - 2]]
  let xm = 0
      ym = height - 1
      initialBoulders = filter (\(V2 x y) -> x /= (width `div` 2)) (take (width * height `div` 3) allCoords)
      initialEarths = filter (`notElem` initialBoulders) initEarth
  return $ Game
      { _miner  = V2 xm ym
      , _minerRestTickConst = 2
      , _minerRestTick = 2
      , _monsterRestTickConst = 5
      , _monsterRestTick = 5
      , _score  = 0
      , _dead   = False
      , _win    = False
      , _bomb = V2 0 0
      , _earths = initialEarths
      , _boulders = initialBoulders
      , _bombTickConst = 15
      , _bombTick = 15
      , _bombState = -1
      , _minerDirc = North
      , _blastCrossRadius = 2
      , _blastSquareRadius = 0
      , _blastCircleRadius = 0
      , _monster = V2 (width `div` 2) 0
      , _monsterHP = 5
      , _bombType = 0
      }