{-# LANGUAGE NoImplicitPrelude #-}
module Jumpie.Types (
    OutgoingAction(..),
    PointReal,
    RectReal,
    LineSegmentReal,
    LineSegmentInt,
    Real,
    RectInt,
    PointInt,
    Keydowns,
    isStarCollected,
    ) where

import           Jumpie.Geometry.LineSegment (LineSegment)
import           Jumpie.Geometry.Point (Point2)
import           Jumpie.Geometry.Rect (Rect())
import           Wrench.Keysym (Keysym)
import           ClassyPrelude hiding (Real)

type RectInt = Rect (Point2 Int)

type Real = Double
type PointInt = Point2 Int
type PointReal = Point2 Real
type RectReal = Rect PointReal
type LineSegmentReal = LineSegment PointReal
type LineSegmentInt = LineSegment PointInt

data OutgoingAction = StarCollected

isStarCollected :: OutgoingAction -> Bool
isStarCollected StarCollected = True

type Keydowns = Set Keysym
