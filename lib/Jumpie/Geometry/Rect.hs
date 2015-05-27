{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
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
import Linear.V2(V2(..),_x,_y)
import Linear.Vector((*^))
import Control.Lens((^.),view)
import Control.Lens.Getter(to,Getter)
import Control.Lens.TH(makeLenses)
import ClassyPrelude

data Rect a = Rect {
    _rectTopLeft :: a
  , _rectBottomRight :: a
  } deriving(Show,Functor)

$(makeLenses ''Rect)

-- TODO: pointToList is a Lens.
inside :: Ord a => Rect (Point2 a) -> Rect (Point2 a) -> Bool
inside (Rect tlsmall brsmall) (Rect tlbig brbig) = (pointToList tlsmall >= pointToList tlbig) && (pointToList brsmall <= pointToList brbig)

lineSegments :: Getter (Rect (Point2 a)) [LineSegment (Point2 a)]
lineSegments = to helper
  where
    helper (Rect tl br) = [
        LineSegment (V2  _left _top) (V2  _right _top),
        LineSegment (V2  _right _top) (V2  _right _bottom),
        LineSegment (V2  _right _bottom) (V2  _left _bottom),
        LineSegment (V2  _left _bottom) (V2  _left _top)]
        where _left = tl ^. _x
              _right = br ^. _x
              _top = tl ^. _y
              _bottom = br ^. _y

dimensions :: Num a => Getter (Rect (Point2 a)) (Point2 a)
dimensions = to (\(Rect tl br) -> br - tl)

center :: Fractional a => Getter (Rect (Point2 a)) (Point2 a)
center = to (\r -> r ^. rectTopLeft + 0.5 *^ (r ^. dimensions))

top, left, right, bottom :: Getter (Rect (Point2 a)) a
top = to (view (rectTopLeft . _y))
left = to (view (rectTopLeft . _x))
bottom = to (view (rectBottomRight . _y))
right = to (view (rectBottomRight . _x))

rectToTuple :: Rect (Point2 a) -> (Point2 a,Point2 a)
rectToTuple (Rect a b) = (a,b)
