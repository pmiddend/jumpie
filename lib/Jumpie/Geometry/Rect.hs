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

import Jumpie.Geometry.Point
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Data.Ord(Ord,(>=),(<=))
import Prelude((-),Num,Fractional,(+))
import Data.Functor(Functor,fmap)
import Data.Bool(Bool,(&&))
import Text.Show(Show)
import Linear.V2(V2(..),_x,_y)
import Linear.Vector((*^))
import Control.Lens((^.))

data Rect a = Rect { rectTopLeft :: a, rectBottomRight :: a } deriving(Show)

instance Functor Rect where
  fmap f (Rect a b) = Rect (f a) (f b)

inside :: Ord a => Rect (Point2 a) -> Rect (Point2 a) -> Bool
inside (Rect tlsmall brsmall) (Rect tlbig brbig) = (pointToList tlsmall >= pointToList tlbig) && (pointToList brsmall <= pointToList brbig)

lineSegments :: Rect (Point2 a) -> [LineSegment (Point2 a)]
lineSegments (Rect tl br) = [
  LineSegment (V2  _left _top) (V2  _right _top),
  LineSegment (V2  _right _top) (V2  _right _bottom),
  LineSegment (V2  _right _bottom) (V2  _left _bottom),
  LineSegment (V2  _left _bottom) (V2  _left _top)]
  where _left = tl ^. _x
        _right = br ^. _x
        _top = tl ^. _y
        _bottom = br ^. _y

dimensions :: Num a => Rect (Point2 a) -> Point2 a
dimensions (Rect tl br) = br - tl

center :: Fractional a => Rect (Point2 a) -> Point2 a
center r@(Rect lt _) = lt + 0.5 *^ dimensions r

top, left, right, bottom :: Rect (Point2 a) -> a
top (Rect (V2  _ a) _) = a
left (Rect (V2  a _) _) = a
bottom (Rect _ (V2  _ a)) = a
right (Rect _ (V2  a _)) = a

rectToTuple :: Rect (Point2 a) -> (Point2 a,Point2 a)
rectToTuple (Rect a b) = (a,b)
