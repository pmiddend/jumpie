module Jumpie.Bresenham(bresenham) where

import Data.List(sort,unfoldr,map)
import Data.Function((.),($),id)
import Prelude(abs,(-),(+),(*),Num)
import Data.Ord((>),(<),(>=),Ord)
import Data.Maybe(Maybe(..))
import Data.Bool(otherwise)
import Jumpie.Geometry.Point(Point(..),getX,getY,toTuple)
import Jumpie.Geometry.LineSegment(LineSegment(..))

bresenham :: (Num a,Ord a) => LineSegment (Point a) -> [Point a]
bresenham (LineSegment pa pb) = map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs ((getY pb) - (getY pa)) > abs ((getX pb) - (getX pa))
    maySwitch = if steep then (\(Point x y) -> Point y x) else id
    [(x1,y1),(x2,y2)] = (sort . map toTuple) [maySwitch pa, maySwitch pb]
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, error)
        | xTemp > x2 = Nothing
        | otherwise  = Just (Point  xTemp yTemp, (xTemp + 1, newY, newError))
        where
          tempError = error + deltay
          (newY, newError) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)
