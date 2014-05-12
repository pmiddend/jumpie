module Jumpie.Types(
  TimeDelta(TimeDelta),
  RectReal,
  timeDelta,
  IncomingAction(..),
  GameTicks(GameTicks),
  tickValue,
  LineSegmentReal,
  OutgoingAction(..),
  PointReal,
  Real,
  RectInt,
  Keydowns) where

import Data.Eq(Eq)
import Data.Int(Int)
import Data.Word(Word64)
import Graphics.UI.SDL.Keysym(SDLKey)
import Jumpie.Geometry.LineSegment(LineSegment)
import Jumpie.Geometry.Point(Point2(..))
import Jumpie.Geometry.Rect(Rect())
import Prelude(Double)

newtype TimeDelta = TimeDelta { timeDelta :: Double }

newtype GameTicks = GameTicks { tickValue :: Word64 }

type RectInt = Rect (Point2 Int)

type Real = Double
type PointReal = Point2 Real
type RectReal = Rect PointReal
type LineSegmentReal = LineSegment PointReal

data OutgoingAction = Collision

data IncomingAction = PlayerLeft | PlayerRight | PlayerJump deriving(Eq)

type Keydowns = [SDLKey]
