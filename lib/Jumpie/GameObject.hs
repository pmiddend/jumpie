{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Jumpie.GameObject(
  GameObject(..),
  PlayerMode(..),
  BoxType(..),
  Player(..),
  SensorLine(SensorLine),
  Particle(..),
  MoveableObject(..),
  line,
  maybeBox,
  playerPosition,
  playerMode,
  playerVelocity,
  playerWalkSince,
  boxPosition,
  boxDeadline,
  particleInception,
  particleIdentifier,
  _ObjectBox,
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
import Control.Lens.TH
import Control.Lens((&),(+~),(%~))

data PlayerMode = Ground | Air deriving(Eq,Show)

class MoveableObject p where
  moveObject :: p -> PointReal -> p

data Player = Player {
    _playerPosition  :: PointReal
  , _playerMode      :: PlayerMode
  , _playerVelocity  :: PointReal
  , _playerWalkSince :: Maybe TimeTicks
  } deriving(Show)

$(makeClassy ''Player)

instance MoveableObject Player where
  moveObject p v = p & playerPosition +~ v

data Particle =
       Particle
         { _particleIdentifier :: AnimId
         , _particlePosition :: PointReal
         , _particleInception :: TimeTicks
         }
  deriving Show

$(makeClassy ''Particle)

instance MoveableObject Particle where
  moveObject p v = p & particlePosition +~ v

newtype SensorLine = SensorLine { line :: LineSegment PointReal }
  deriving Show

data BoxType = BoxLeft
             | BoxRight
             | BoxSingleton
             | BoxMiddle
  deriving Show

data Box = Box {
    _boxPosition :: RectReal
  , _boxDeadline :: TimeTicks
  , _boxType :: BoxType
  }
  deriving Show

$(makeClassy ''Box)

instance MoveableObject Box where
  moveObject p v = p & boxPosition %~ (\o -> moveObject o v)

instance MoveableObject RectReal where
  moveObject p v = p & rectTopLeft +~ v & rectBottomRight +~ v

instance MoveableObject (LineSegment PointReal) where
  moveObject p v = p & lineSegmentFrom +~ v & lineSegmentTo +~ v

instance MoveableObject SensorLine where
  moveObject p v = SensorLine (moveObject (line p) v)

data GameObject = ObjectPlayer Player
                | ObjectBox Box
                | ObjectSensorLine SensorLine
                | ObjectParticle Particle
  deriving(Show)

$(makePrisms ''GameObject)

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
