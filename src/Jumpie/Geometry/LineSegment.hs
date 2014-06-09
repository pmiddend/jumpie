module Jumpie.Geometry.LineSegment(
  LineSegment(LineSegment),
  lineSegmentFrom,
  lineSegmentTo,
  pointList) where

import Data.Functor(Functor,fmap)
import Text.Show(Show)
import Data.Eq(Eq)

data LineSegment a = LineSegment { lineSegmentFrom :: a,lineSegmentTo :: a } deriving(Show,Eq)

instance Functor LineSegment where
  fmap f (LineSegment a b) = LineSegment (f a) (f b)

pointList :: LineSegment a -> [a]
pointList (LineSegment a b) = [a,b]
