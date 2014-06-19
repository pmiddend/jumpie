module Jumpie.Types(
  IncomingAction(..),
  OutgoingAction(..),
  PointReal,
  RectReal,
  LineSegmentReal,
  LineSegmentInt,
  Real,
  RectInt,
  PointInt,
  Keydowns) where

import           Data.Eq                     (Eq)
import           Data.Int                    (Int)
import           Graphics.UI.SDL.Enum        (Scancode)
import           Jumpie.Geometry.LineSegment (LineSegment)
import           Jumpie.Geometry.Point       (Point2 (..))
import           Jumpie.Geometry.Rect        (Rect ())
import           Prelude                     (Double)

type RectInt = Rect (Point2 Int)

type Real = Double
type PointInt = Point2 Int
type PointReal = Point2 Real
type RectReal = Rect PointReal
type LineSegmentReal = LineSegment PointReal
type LineSegmentInt = LineSegment PointInt

data OutgoingAction = StarCollected

data IncomingAction = PlayerLeft | PlayerRight | PlayerJump deriving(Eq)

type Keydowns = [Scancode]
