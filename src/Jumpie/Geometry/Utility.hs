module Jumpie.Geometry.Utility(
  clamp,
  clampAbs
  ) where

import Prelude(Num,negate)
import Data.Ord(Ord,(<),(>))

clamp :: Ord a => a -> a -> a -> a
clamp low high v = if v < low then low else if v > high then high else v

clampAbs :: (Num a,Ord a) => a -> a -> a
clampAbs a v = clamp (negate a) a v
