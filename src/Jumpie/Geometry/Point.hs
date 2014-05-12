module Jumpie.Geometry.Point(
  Point2(Point2),
  pX,
  pY,
  cross,
  dot,
  vmult,
  fromTuple,
  toTuple,
  toList) where

import Prelude(Num,(+),(*),(-),negate,abs,signum,fromInteger)
import Data.Functor(fmap,Functor)
import Control.Applicative(Applicative,(<*>),pure,liftA2)
import Text.Show(Show)

data Point2 a = Point2 { pX :: a, pY :: a } deriving(Show)

instance Functor Point2 where
  fmap f (Point2 a b) = Point2 (f a) (f b)

instance Applicative Point2 where
  pure s = Point2 s s
  pf <*> pa = Point2 ((pX pf) (pX pa)) ((pY pf) (pY pa))

instance Num a => Num (Point2 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger i = Point2 (fromInteger i) (fromInteger i)

cross :: Num a => Point2 a -> Point2 a -> a
cross (Point2 x1 y1) (Point2 x2 y2) = x1 * y2 - y1 * x2

dot :: Num a => Point2 a -> Point2 a -> a
dot (Point2 x1 y1) (Point2 x2 y2) = x1 * x2 + y1 * y2

vmult :: Num a => a -> Point2 a -> Point2 a
vmult s p = fmap (s *) p

toTuple :: Point2 a -> (a,a)
toTuple (Point2 x y) = (x,y)

fromTuple :: (a,a) -> Point2 a
fromTuple (a,b) = Point2 a b

toList :: Point2 a -> [a]
toList (Point2 a b) = [a,b]
