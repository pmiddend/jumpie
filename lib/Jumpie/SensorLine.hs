module Jumpie.SensorLine where

import ClassyPrelude
import Jumpie.Types
import Jumpie.MoveableObject
import Jumpie.TileIncrement
import Control.Lens((&),(+~))
import Linear.V2(_x)
import Jumpie.Geometry.LineSegment

newtype SensorLine = SensorLine { line :: LineSegment PointReal }
  deriving Show

instance MoveableObject SensorLine where
  moveObject (SensorLine p) v = SensorLine (p & lineSegmentFrom . _x +~ tileIncrementAbsReal v & lineSegmentTo . _x +~ tileIncrementAbsReal v)
