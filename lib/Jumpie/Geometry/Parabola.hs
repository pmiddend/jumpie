module Jumpie.Geometry.Parabola(
  Parabola(Parabola),
  paraZenith,
  paraBounds,
  paraY,
  paraInvert
  ) where

import Text.Show(Show)
import Prelude(negate,(/),(*),Fractional,sqrt,(+),Floating,(-),Num)
import Data.Function(($))
import Jumpie.Tuple(both)

newtype Parabola a = Parabola (a,a,a) deriving(Show)

paraZenith :: Fractional a => Parabola a -> a
paraZenith (Parabola (a,b,_)) = negate (b / (2 * a))

paraBounds :: Floating a => Parabola a -> a -> (a,a)
paraBounds (Parabola (a,b,c)) y = both (/(2*a)) (negate b + r,negate (b + r))
  where r = sqrt $ b*b - 4*a*c + 4*a*y

-- Achtung! Klappt nur, wenn c == 0
paraInvert :: Num a => Parabola a -> Parabola a
paraInvert (Parabola (a,b,c)) = Parabola (a,negate b,c)

paraY :: Num a => Parabola a -> a -> a
paraY (Parabola (a,b,c)) x = a*x*x + b*x + c
