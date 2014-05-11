module Jumpie.Geometry.LineSegment(
  LineSegment(LineSegment),
  from,
  to,
  pointList) where

import Data.Functor(Functor,fmap)

data LineSegment a = LineSegment { from :: a,to :: a }

instance Functor LineSegment where
  fmap f (LineSegment a b) = LineSegment (f a) (f b)

pointList :: LineSegment a -> [a]
pointList (LineSegment a b) = [a,b]
