module NewPoint where

class Point p where
  vmult :: Num a => a -> p a -> p a
  toList :: Num a => p a -> [a]

instance Point [] where
  vmult a = fmap (*a)
  toList = id

scalar :: (Num a,Point p) => p a -> p a -> a
scalar a b = foldr (+) 0 $ zipWith (*) (toList a) (toList b)
