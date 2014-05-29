module Jumpie.Geometry.Rect(
  Rect(Rect),
  rectTopLeft,
  rectBottomRight,
  inside,
  lineSegments,
  dimensions,
  center,
  top,
  bottom,
  left,
  right,
  rectToTuple
  ) where

import Jumpie.Geometry.Point(Point2(Point2),pX,pY,pointToList,vmult)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Data.Ord(Ord,(>=),(<=))
import Prelude((-),Num,Fractional,(+))
import Data.Functor(Functor,fmap)
import Data.Bool(Bool,(&&))
import Text.Show(Show)

data Rect a = Rect { rectTopLeft :: a, rectBottomRight :: a } deriving(Show)

instance Functor Rect where
  fmap f (Rect a b) = Rect (f a) (f b)

inside :: Ord a => Rect (Point2 a) -> Rect (Point2 a) -> Bool
inside (Rect tlsmall brsmall) (Rect tlbig brbig) = (pointToList tlsmall >= pointToList tlbig) && (pointToList brsmall <= pointToList brbig)

lineSegments :: Rect (Point2 a) -> [LineSegment (Point2 a)]
lineSegments (Rect tl br) = [
  LineSegment (Point2 _left _top) (Point2 _right _top),
  LineSegment (Point2 _right _top) (Point2 _right _bottom),
  LineSegment (Point2 _right _bottom) (Point2 _left _bottom),
  LineSegment (Point2 _left _bottom) (Point2 _left _top)]
  where _left = pX tl
        _right = pX br
        _top = pY tl
        _bottom = pY br

dimensions :: Num a => Rect (Point2 a) -> Point2 a
dimensions (Rect tl br) = br - tl

center :: Fractional a => Rect (Point2 a) -> Point2 a
center r@(Rect lt _) = lt + 0.5 `vmult` dimensions r

top, left, right, bottom :: Rect (Point2 a) -> a
top (Rect (Point2 _ a) _) = a
left (Rect (Point2 a _) _) = a
bottom (Rect _ (Point2 _ a)) = a
right (Rect _ (Point2 a _)) = a

rectToTuple :: Rect (Point2 a) -> (Point2 a,Point2 a)
rectToTuple (Rect a b) = (a,b)
