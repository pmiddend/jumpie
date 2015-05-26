module Jumpie.Tuple(
  both,
  between
  ) where

import Data.Ord(Ord,(>=),(<=))
import Data.Bool(Bool,(&&))

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a,f b)

between :: Ord a => a -> (a,a) -> Bool
between p (l,r) = p >= l && p <= r
