module Jumpie.Geometry.Intersection(
  rectIntersects,
  lineSegmentIntersection,
  rectLineSegmentIntersects,
  pointInsideRect,
  lineSegmentIntersects
  ) where

import Control.Applicative((<$>),(<*>),pure)
import Data.Bool((&&),(||),not,otherwise,Bool)
import Data.Composition((.:))
import Data.Function((.),($))
import Data.List(or,map,zip,zipWith,and,or)
import Data.Maybe(Maybe(..),isJust)
import Data.Ord((<=),(>=),(>),Ord)
import Jumpie.Geometry.LineSegment(LineSegment(..),pointList)
import Jumpie.Geometry.Point(Point2,vmult,cross,dot,toList)
import Jumpie.Geometry.Rect(Rect,inside,lineSegments,topLeft,bottomRight)
import Prelude(Num,(+),(*),(-),(/),negate,abs,signum,fromInteger,Double,Fractional)

lineSegmentIntersects :: (Num a,Fractional a,Ord a) => a -> LineSegment (Point2 a) -> LineSegment (Point2 a) -> Bool
lineSegmentIntersects delta l1 l2 = isJust $ lineSegmentIntersection delta l1 l2

-- Kleiner Hinweis: hier ist fast gar kein (Point2 a) explizit noetig, aber man brauch vmult, dot und cross.
-- Vielleicht kann man das in 'ne Typklasse auslagern?
lineSegmentIntersection :: (Num a,Fractional a,Ord a) => a -> LineSegment (Point2 a) -> LineSegment (Point2 a) -> Maybe (Point2 a)
lineSegmentIntersection delta (LineSegment p to1) (LineSegment q to2)
  | collinear && overlapping = Just (p + (tnom/denom) `vmult` r)
  | collinear && not overlapping = Nothing
  | abs denom <= delta && abs unom > delta = Nothing
  | abs denom > delta && isNormalized (tnom / denom) && isNormalized (unom / denom) = Just (p + (tnom/denom) `vmult` r)
  | otherwise = Nothing
  where r = to1 - p
        s = to2 - q
        denom = r `cross` s
        tnom = (q - p) `cross` s
        unom = (q - p) `cross` r
        isNormalized z = z >= 0 && z <= 1
        collinear = abs denom <= delta && abs unom <= delta
        overlapping = (0 <= ((q - p) `dot` r) && ((q - p) `dot` r) <= (r `dot` r)) || (0 <= (p - q) `dot` s && (p - q) `dot` s <= s `dot` s)

rectIntersects :: (Num a,Fractional a,Ord a) => a -> Rect (Point2 a) -> Rect (Point2 a) -> Bool
rectIntersects delta a b = a `inside` b || b `inside` a || or (isJust .: lineSegmentIntersection delta <$> lineSegments a <*> lineSegments b)

pointInsideRect :: Ord a => Rect (Point2 a) -> Point2 a -> Bool
pointInsideRect r p = and $ zipWith between (zip (toList . topLeft $ r) (toList . bottomRight $ r)) (toList p)
  where between :: Ord a => (a,a) -> a -> Bool
        between (left,right) a = a >= left && a <= right

rectLineSegmentIntersects :: (Num a,Fractional a,Ord a) => a -> Rect (Point2 a) -> LineSegment (Point2 a) -> Bool
rectLineSegmentIntersects delta rect line = or (pure (lineSegmentIntersects delta line) <*> lineSegments rect) || and (map (pointInsideRect rect) (pointList line))
