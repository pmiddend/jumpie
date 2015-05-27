{-# LANGUAGE FlexibleInstances #-}
module Jumpie.GameObject(
  GameObject(..),
  PlayerMode(..),
  BoxType(..),
  Player(Player),
  SensorLine(SensorLine),
  Particle(..),
  MoveableObject(..),
  line,
  maybeBox,
  playerPosition,
  playerMode,
  playerVelocity,
  playerWalkSince,
  Box(..),
  isBox,
  isParticle,
  isPlayer,
  isSensorLine
  ) where

import           Jumpie.Geometry.LineSegment
import           Jumpie.Geometry.Rect
import           Jumpie.Types                (PointReal, RectReal)
import Wrench.Time
import ClassyPrelude hiding(Real)
import Wrench.AnimId

data PlayerMode = Ground | Air deriving(Eq,Show)

class MoveableObject p where
  moveObject :: p -> PointReal -> p

data Player = Player {
    playerPosition  :: PointReal
  , playerMode      :: PlayerMode
  , playerVelocity  :: PointReal
  , playerWalkSince :: Maybe TimeTicks
  } deriving(Show)

instance MoveableObject Player where
  moveObject p v = p { playerPosition = playerPosition p + v }

data Particle =
       Particle
         { particleIdentifier :: AnimId
         , particlePosition :: PointReal
         , particleInception :: TimeTicks
         }
  deriving Show

instance MoveableObject Particle where
  moveObject p v = p { particlePosition = particlePosition p + v }

data GameObject = ObjectPlayer Player
                | ObjectBox Box
                | ObjectSensorLine SensorLine
                | ObjectParticle Particle
  deriving(Show)

instance MoveableObject GameObject where
  moveObject (ObjectPlayer p) v = ObjectPlayer (moveObject p v)
  moveObject (ObjectBox p) v = ObjectBox (moveObject p v)
  moveObject (ObjectSensorLine p) v = ObjectSensorLine (moveObject p v)
  moveObject (ObjectParticle p) v = ObjectParticle (moveObject p v)

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

isParticle :: GameObject -> Bool
isParticle (ObjectParticle _) = True
isParticle _ = False

data BoxType = BoxLeft
             | BoxRight
             | BoxSingleton
             | BoxMiddle
  deriving Show

data Box = Box {
    boxPosition :: RectReal
  , boxDeadline :: TimeTicks
  , boxType :: BoxType
  }
  deriving Show

instance MoveableObject Box where
  moveObject p v = p { boxPosition = moveObject (boxPosition p) v }

instance MoveableObject RectReal where
  moveObject p v = Rect { rectTopLeft = rectTopLeft p + v,rectBottomRight = rectBottomRight p + v }

newtype SensorLine = SensorLine { line :: LineSegment PointReal }
  deriving Show

instance MoveableObject (LineSegment PointReal) where
  moveObject p v = LineSegment (lineSegmentFrom p + v) (lineSegmentTo p + v)

instance MoveableObject SensorLine where
  moveObject p v = SensorLine (moveObject (line p) v)
