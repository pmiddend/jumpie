module Jumpie.GameObject(
  GameObject(..),
  PlayerMode(..),
  BoxType(..),
  Player(Player),
  SensorLine(SensorLine),
  line,
  playerPosition,
  playerMode,
  playerVelocity,
  playerWalkSince,
  Box(Box),
  boxPosition,
  boxType,
  isBox,
  isPlayer,
  isSensorLine
  ) where

import Data.Eq(Eq)
import Jumpie.Types(PointReal,RectReal)
import Jumpie.Time(GameTicks)
import Jumpie.Geometry.LineSegment(LineSegment)
import Data.Maybe(Maybe)
import Data.Bool(Bool(..))

data PlayerMode = Ground | Air deriving(Eq)

data Player = Player {
  playerPosition :: PointReal,
  playerMode :: PlayerMode,
  playerVelocity :: PointReal,
  playerWalkSince :: Maybe GameTicks
  }

data GameObject = ObjectPlayer Player | ObjectBox Box | ObjectSensorLine SensorLine

isBox :: GameObject -> Bool
isBox (ObjectBox _) = True
isBox _ = False

isPlayer :: GameObject -> Bool
isPlayer (ObjectPlayer _) = True
isPlayer _ = False

isSensorLine :: GameObject -> Bool
isSensorLine (ObjectSensorLine _) = True
isSensorLine _ = False

data BoxType = BoxLeft | BoxRight | BoxSingleton | BoxMiddle

data Box = Box { boxPosition :: RectReal,boxType :: BoxType }

newtype SensorLine = SensorLine { line :: LineSegment PointReal }