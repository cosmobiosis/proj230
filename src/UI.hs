{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Miner

import qualified Brick as B
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
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = MinerN | MinerS | MinerW | MinerE | Empty | Earth | Boulder | Bomb0 | Bomb1 | Bomb2 | Blast

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ gameTickAction g

handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ minerMove g North
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ minerMove g South
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ minerMove g West
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ minerMove g East

handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') []))       = continue $ minerMove g North
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') []))       = continue $ minerMove g South
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') []))       = continue $ minerMove g West
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') []))       = continue $ minerMove g East
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') []))       = continue $ minerPutBomb g

-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
-- handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Miner")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c == g ^. miner = case g ^. minerDirc of
          North -> MinerN
          South -> MinerS
          East -> MinerE
          West -> MinerW
      | c == g ^. bomb && g ^. bombState /= -1 = case g ^. bombState of
        0 -> Bomb0
        1 -> Bomb1
        2 -> Bomb2
        _ -> Empty
      | g ^. bombState == 2 && inBlastRange g c = Blast
      | c `elem` g ^. boulders = Boulder
      | c `elem` g ^. earths = Earth
      | otherwise       = Empty

drawCell :: Cell -> Widget Name
drawCell MinerN = withAttr minerNAttr arrowN
drawCell MinerS = withAttr minerSAttr arrowS
drawCell MinerW = withAttr minerWAttr arrowW
drawCell MinerE = withAttr minerEAttr arrowE
drawCell Bomb0  = withAttr bomb0Attr  smallCircle
drawCell Bomb1  = withAttr bomb1Attr  largeCircle
drawCell Bomb2  = withAttr bomb2Attr  square
drawCell Blast = withAttr  blastAttr  square
drawCell Earth  = withAttr earthAttr  square
drawCell Boulder = withAttr boulderAttr square
drawCell Empty = withAttr  emptyAttr  square

square :: Widget Name
square = str "   "

arrowN :: Widget Name
arrowN = str " N "

arrowS :: Widget Name
arrowS = str " S "

arrowW :: Widget Name
arrowW = str " W "

arrowE :: Widget Name
arrowE = str " E "

smallCircle :: Widget Name
smallCircle = str " o "

largeCircle :: Widget Name
largeCircle = str " Q "

bbox :: Widget Name
bbox = str " B "

xbox ::  Widget Name
xbox = str " X "

minerColor :: V.Attr
minerColor = V.black `on` V.green

earthColor :: V.Color
earthColor = V.rgbColor 111 78 55

boulderColor :: V.Color
boulderColor = V.rgbColor 124 129 124

theMap :: B.AttrMap
theMap = B.attrMap V.defAttr
  [
    (minerNAttr, minerColor),
    (minerSAttr, minerColor),
    (minerWAttr, minerColor),
    (minerEAttr, minerColor),
    (bomb2Attr, V.red `on` V.red),
    (blastAttr, V.yellow `on` V.yellow),
    (earthAttr, earthColor `on` earthColor),
    (boulderAttr, boulderColor `on` boulderColor)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

minerNAttr, minerSAttr, minerWAttr, minerEAttr, bomb0Attr, bomb1Attr, emptyAttr :: AttrName
minerNAttr = "minerNAttr"
minerSAttr = "minerSAttr"
minerWAttr = "minerWAttr"
minerEAttr = "minerWAttr"
bomb0Attr  = "bomb0Attr"
bomb1Attr  = "bomb1Attr"
bomb2Attr  = "bomb2Attr"
blastAttr  = "blastAttr"
boulderAttr = "boulderAttr"
earthAttr = "earthAttr"
emptyAttr  = "emptyAttr"
