module Jumpie.Geometry.LineSegment(LineSegment(LineSegment),getFrom,getTo) where

data LineSegment a = LineSegment { getFrom :: a,getTo :: a }

instance Functor LineSegment where
  fmap f (LineSegment a b) = LineSegment (f a) (f b)
