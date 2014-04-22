module Jumpie.Geometry.Rect(Rect(Rect),getTopLeft,getBottomRight,inside,lineSegments) where

import Jumpie.Geometry.Point(Point(Point),getX,getY,toList)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Data.Ord(Ord,(>=),(<=))
import Prelude()
import Data.Functor(Functor,fmap)
import Data.Bool(Bool,(&&))

data Rect a = Rect { getTopLeft :: a, getBottomRight :: a }

instance Functor Rect where
  fmap f (Rect a b) = Rect (f a) (f b)

inside :: Ord a => Rect (Point a) -> Rect (Point a) -> Bool
inside (Rect tlsmall brsmall) (Rect tlbig brbig) = (toList tlsmall >= toList tlbig) && (toList brsmall <= toList brbig)

lineSegments :: Rect (Point a) -> [LineSegment (Point a)]
lineSegments (Rect tl br) = [
  LineSegment (Point left top) (Point right top),
  LineSegment (Point right top) (Point right bottom),
  LineSegment (Point right bottom) (Point left bottom),
  LineSegment (Point left bottom) (Point left top)]
  where left = getX tl
        right = getX br
        top = getY tl
        bottom = getY br

