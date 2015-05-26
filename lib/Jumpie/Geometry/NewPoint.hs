{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}
module NewPoint where

import Prelude((*),(+))

class VectorSpace a s where
  vmult :: s -> a -> a
  (.+.) :: a -> a -> a
  inverse :: a -> a
  --(.-.) :: a -> a -> a
  --a .-. b = a .+. (inverse b)

data Point2 a = Point2 a a

instance VectorSpace (Point2 a) s where
  s `vmult` (Point2 x y) = Point2 (s*x) (s*y)
  (Point2 x1 y1) .+. (Point2 x2 y2) = Point2 (x1+x2) (y1*y2)
  inverse (Point2 x y) = Point2 (-x) (-y)
{-
class Point p where
  vmult :: Num a => a -> p a -> p a
  toList :: Num a => p a -> [a]

instance Point [] where
  vmult a = fmap (*a)
  toList = id

scalar :: (Num a,Point p) => p a -> p a -> a
scalar a b = foldr (+) 0 $ zipWith (*) (toList a) (toList b)
-}
