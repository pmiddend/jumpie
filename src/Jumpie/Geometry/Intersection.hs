module Jumpie.Geometry.Intersection(intersection) where

import Prelude(Num,(+),(*),(-),(/),negate,abs,signum,fromInteger,Double,Fractional)
import Data.Maybe(Maybe(..))
import Data.Bool((&&),(||),not,otherwise)
import Data.Ord((<=),(>=),(>),Ord)
import Jumpie.Geometry.LineSegment(LineSegment(..))
import Jumpie.Geometry.Point(Point,vmult,cross,scalar)

intersection :: (Num a,Fractional a,Ord a) => LineSegment (Point a) -> LineSegment (Point a) -> a -> Maybe (Point a)
intersection (LineSegment p to1) (LineSegment q to2) delta
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
        overlapping = (0 <= ((q - p) `scalar` r) && ((q - p) `scalar` r) <= (r `scalar` r)) || (0 <= (p - q) `scalar` s && (p - q) `scalar` s <= s `scalar` s)
