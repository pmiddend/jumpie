module Jumpie.Geometry.Point(Point(Point),getX,getY,cross,scalar,vmult,fromTuple,toTuple) where

import Prelude(Num,(+),(*),(-),negate,abs,signum,fromInteger)
import Data.Functor(fmap,Functor)

data Point a = Point { getX :: a, getY :: a }

-- Hilfsfunktion fuer Num unten
biOnComponent :: (a -> a -> a) -> Point a -> Point a -> Point a
biOnComponent f (Point a1 b1) (Point a2 b2) = Point (f a1 a2) (f b1 b2)

instance Functor Point where
  fmap f (Point a b) = Point (f a) (f b)

instance Num a => Num (Point a) where
  (+) = biOnComponent (+)
  (*) = biOnComponent (*)
  (-) = biOnComponent (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger i = Point (fromInteger i) (fromInteger i)

cross :: Num a => Point a -> Point a -> a
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

scalar :: Num a => Point a -> Point a -> a
scalar (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

vmult :: Num a => a -> Point a -> Point a
vmult s p = fmap (s *) p

toTuple :: Point a -> (a,a)
toTuple (Point x y) = (x,y)

fromTuple :: (a,a) -> Point a
fromTuple (a,b) = Point a b
