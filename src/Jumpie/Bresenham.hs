module Jumpie.Bresenham(bresenham) where

import Data.List(sort,unfoldr,map)
import Data.Function((.),($),id)
import Prelude(abs,(-),(+),(*),Num)
import Data.Ord((>),(<),(>=),Ord)
import Data.Maybe(Maybe(..))
import Data.Bool(otherwise)
import Jumpie.Geometry.Point(Point2(..),pX,pY,toTuple)
import Jumpie.Geometry.LineSegment(LineSegment(..))

bresenham :: (Num a,Ord a) => LineSegment (Point2 a) -> [Point2 a]
bresenham (LineSegment pa pb) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs ((pY pb) - (pY pa)) > abs ((pX pb) - (pX pa))
    maySwitch = if steep then (\(Point2 x y) -> Point2 y x) else id
    [(x1,y1),(x2,y2)] = (sort . map toTuple) [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just (Point2 xTemp yTemp, (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)
