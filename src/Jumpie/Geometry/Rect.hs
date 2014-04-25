module Jumpie.Geometry.Rect(
  Rect(Rect),
  topLeft,
  bottomRight,
  inside,
  lineSegments,
  dimensions
  ) where

import Jumpie.Geometry.Point(Point2(Point2),_x,_y,toList)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Data.Ord(Ord,(>=),(<=))
import Prelude((-),Num)
import Data.Functor(Functor,fmap)
import Data.Bool(Bool,(&&))

data Rect a = Rect { topLeft :: a, bottomRight :: a }

instance Functor Rect where
  fmap f (Rect a b) = Rect (f a) (f b)

inside :: Ord a => Rect (Point2 a) -> Rect (Point2 a) -> Bool
inside (Rect tlsmall brsmall) (Rect tlbig brbig) = (toList tlsmall >= toList tlbig) && (toList brsmall <= toList brbig)

lineSegments :: Rect (Point2 a) -> [LineSegment (Point2 a)]
lineSegments (Rect tl br) = [
  LineSegment (Point2 left top) (Point2 right top),
  LineSegment (Point2 right top) (Point2 right bottom),
  LineSegment (Point2 right bottom) (Point2 left bottom),
  LineSegment (Point2 left bottom) (Point2 left top)]
  where left = _x tl
        right = _x br
        top = _y tl
        bottom = _y br

dimensions :: Num a => Rect (Point2 a) -> Point2 a
dimensions (Rect tl br) = br - tl
