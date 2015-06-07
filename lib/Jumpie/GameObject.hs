{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Jumpie.GameObject(
  GameObject(..),
  PlayerMode(..),
  Player(..),
  SensorLine(SensorLine),
  Particle(..),
  TileIncrement(..),
  MoveableObject(..),
  line,
  maybePlatform,
  playerPosition,
  playerMode,
  playerVelocity,
  tileIncrementAbsReal,
  playerWalkSince,
  particleInception,
  particleIdentifier,
  _ObjectPlatform,
  isPlatform,
  isParticle,
  isPlayer,
  isSensorLine
  ) where

import           Jumpie.Geometry.LineSegment
import           Jumpie.GameConfig
import           Jumpie.Types                (PointReal, Real)
import Wrench.Time
import ClassyPrelude hiding(Real)
import Jumpie.Platform
import Wrench.AnimId
import Control.Lens.TH
import Linear.V2
import Control.Lens((&),(+~))

data PlayerMode = Ground | Air deriving(Eq,Show)

newtype TileIncrement = TileIncrement { tileIncrementRelative :: Int } deriving(Num)

tileIncrementAbsReal :: TileIncrement -> Real
tileIncrementAbsReal = fromIntegral . (*gcTileSize) . tileIncrementRelative

class MoveableObject p where
  moveObject :: p -> TileIncrement -> p

data Player = Player {
    _playerPosition  :: PointReal
  , _playerMode      :: PlayerMode
  , _playerVelocity  :: PointReal
  , _playerWalkSince :: Maybe TimeTicks
  } deriving(Show)

$(makeClassy ''Player)

instance MoveableObject Player where
  moveObject p v = p & playerPosition . _x +~ (tileIncrementAbsReal v)

data Particle =
       Particle
         { _particleIdentifier :: AnimId
         , _particlePosition :: PointReal
         , _particleInception :: TimeTicks
         }
  deriving Show

$(makeClassy ''Particle)

instance MoveableObject Particle where
  moveObject p v = p & particlePosition . _x +~ (tileIncrementAbsReal v)

newtype SensorLine = SensorLine { line :: LineSegment PointReal }
  deriving Show

{-        
data BoxType = BoxLeft
             | BoxRight
             | BoxSingleton
             | BoxMiddle
  deriving Show
-}

instance MoveableObject Platform where
  moveObject p v = p & platLeft +~ tileIncrementRelative v

{-
instance MoveableObject RectReal where
  moveObject p v = p & rectTopLeft . _x +~ v & rectBottomRight . _x +~ v
-}

instance MoveableObject (LineSegment PointReal) where
  moveObject p v = p & lineSegmentFrom . _x +~ tileIncrementAbsReal v & lineSegmentTo . _x +~ tileIncrementAbsReal v

instance MoveableObject SensorLine where
  moveObject p v = SensorLine (moveObject (line p) v)

data GameObject = ObjectPlayer Player
                | ObjectPlatform Platform
                | ObjectSensorLine SensorLine
                | ObjectParticle Particle
  deriving(Show)

$(makePrisms ''GameObject)

instance MoveableObject GameObject where
  moveObject (ObjectPlayer p) v = ObjectPlayer (moveObject p v)
  moveObject (ObjectPlatform p) v = ObjectPlatform (moveObject p v)
  moveObject (ObjectSensorLine p) v = ObjectSensorLine (moveObject p v)
  moveObject (ObjectParticle p) v = ObjectParticle (moveObject p v)

maybePlatform :: GameObject -> Maybe Platform
maybePlatform (ObjectPlatform b) = Just b
maybePlatform _ = Nothing

isPlatform :: GameObject -> Bool
isPlatform (ObjectPlatform _) = True
isPlatform _ = False

isPlayer :: GameObject -> Bool
isPlayer (ObjectPlayer _) = True
isPlayer _ = False

isSensorLine :: GameObject -> Bool
isSensorLine (ObjectSensorLine _) = True
isSensorLine _ = False

isParticle :: GameObject -> Bool
isParticle (ObjectParticle _) = True
isParticle _ = False
