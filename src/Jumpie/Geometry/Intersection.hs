module Jumpie.Geometry.Intersection(rectIntersects,lineSegmentIntersection) where

import Prelude(Num,(+),(*),(-),(/),negate,abs,signum,fromInteger,Double,Fractional)
import Data.Maybe(Maybe(..),isJust)
import Data.Bool((&&),(||),not,otherwise,Bool)
import Data.Ord((<=),(>=),(>),Ord)
import Data.List(or,map)
import Data.Function((.),($))
import Jumpie.Geometry.LineSegment(LineSegment(..))
import Jumpie.Geometry.Point(Point,vmult,cross,scalar)
import Jumpie.Geometry.Rect(Rect,inside,lineSegments)
import Control.Applicative((<$>),(<*>))
import Data.Composition((.:))

-- Kleiner Hinweis: hier ist fast gar kein (Point a) explizit noetig, aber man brauch vmult, scalar und cross.
-- Vielleicht kann man das in 'ne Typklasse auslagern?
lineSegmentIntersection :: (Num a,Fractional a,Ord a) => a -> LineSegment (Point a) -> LineSegment (Point a) -> Maybe (Point a)
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
        overlapping = (0 <= ((q - p) `scalar` r) && ((q - p) `scalar` r) <= (r `scalar` r)) || (0 <= (p - q) `scalar` s && (p - q) `scalar` s <= s `scalar` s)

rectIntersects :: (Num a,Fractional a,Ord a) => a -> Rect (Point a) -> Rect (Point a) -> Bool
rectIntersects delta a b = a `inside` b || b `inside` a || or (isJust .: lineSegmentIntersection delta <$> lineSegments a <*> lineSegments b)
