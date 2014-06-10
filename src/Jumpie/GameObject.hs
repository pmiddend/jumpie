module Jumpie.GameObject(
  GameObject(..),
  PlayerMode(..),
  BoxType(..),
  Player(Player),
  SensorLine(SensorLine),
  Star(..),
  line,
  maybeBox,
  playerPosition,
  playerMode,
  playerVelocity,
  playerWalkSince,
  Box(Box),
  boxPosition,
  boxType,
  isBox,
  isStar,
  isPlayer,
  isSensorLine
  ) where

import           Data.Bool                   (Bool (..))
import           Data.Eq                     (Eq)
import           Data.Maybe                  (Maybe (..))
import           Jumpie.Geometry.LineSegment (LineSegment)
import           Jumpie.Time                 (GameTicks)
import           Jumpie.Types                (PointReal, RectReal)
import           Text.Show                   (Show)

data PlayerMode = Ground | Air deriving(Eq,Show)

data Player = Player {
  playerPosition  :: PointReal,
  playerMode      :: PlayerMode,
  playerVelocity  :: PointReal,
  playerWalkSince :: Maybe GameTicks
  } deriving(Show)

data Star = Star {
  starPosition  :: PointReal,
  starInception :: GameTicks
  } deriving(Show)

data GameObject = ObjectPlayer Player | ObjectBox Box | ObjectSensorLine SensorLine | ObjectStar Star deriving(Show)

maybeBox :: GameObject -> Maybe Box
maybeBox (ObjectBox b) = Just b
maybeBox _ = Nothing

isBox :: GameObject -> Bool
isBox (ObjectBox _) = True
isBox _ = False

isPlayer :: GameObject -> Bool
isPlayer (ObjectPlayer _) = True
isPlayer _ = False

isSensorLine :: GameObject -> Bool
isSensorLine (ObjectSensorLine _) = True
isSensorLine _ = False

isStar :: GameObject -> Bool
isStar (ObjectStar _) = True
isStar _ = False

data BoxType = BoxLeft | BoxRight | BoxSingleton | BoxMiddle deriving(Show)

data Box = Box { boxPosition :: RectReal,boxType :: BoxType } deriving(Show)

newtype SensorLine = SensorLine { line :: LineSegment PointReal } deriving(Show)
