module Jumpie.Geometry.Point(Point(Point),getX,getY,cross,scalar,vmult,fromTuple,toTuple,toList) where

import Prelude(Num,(+),(*),(-),negate,abs,signum,fromInteger)
import Data.Functor(fmap,Functor)
import Control.Applicative(Applicative,(<*>),pure,liftA2)

data Point a = Point { getX :: a, getY :: a }

instance Functor Point where
  fmap f (Point a b) = Point (f a) (f b)

instance Applicative Point where
  pure s = Point s s
  pf <*> pa = Point ((getX pf) (getX pa)) ((getY pf) (getY pa))

instance Num a => Num (Point a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger i = Point (fromInteger i) (fromInteger i)

cross :: Num a => Point a -> Point a -> a
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

dot :: Num a => Point a -> Point a -> a
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

vmult :: Num a => a -> Point a -> Point a
vmult s p = fmap (s *) p

toTuple :: Point a -> (a,a)
toTuple (Point x y) = (x,y)

fromTuple :: (a,a) -> Point a
fromTuple (a,b) = Point a b

toList :: Point a -> [a]
toList (Point a b) = [a,b]
