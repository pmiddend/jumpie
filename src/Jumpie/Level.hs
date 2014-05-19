module Jumpie.Level(
  Platform(Platform),
  validPlatforms,
  randomPlatform,
  showPlatforms,
  replaceNth,
  setPartList,
  showPlatformsPpm,
  randomPlatforms) where

import System.Random(Random,randomR,RandomGen)
import Jumpie.Geometry.Point(Point2(Point2))
import Jumpie.Debug(traceShowId)
import Jumpie.Geometry.Rect(Rect(Rect),dimensions)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Jumpie.Types(RectInt,PointInt,LineSegmentReal,PointReal,Real)
import Jumpie.GameConfig(gcGrv,gcJmp,gcPlayerMaxSpeed,gcTileSize)
import Data.Bool((||),Bool,(&&))
import Control.Category((>>>))
import Data.Eq((==))
import Data.Function((.),($))
import Data.Tuple(uncurry)
import Prelude((*),sqrt,(-),(/),(+),fromIntegral,otherwise)
import Data.List(or,(!!),(++),replicate,intersperse,unlines)
import Data.Int(Int)
import Data.String(String)
import Data.Ord(Ord,(<=),(>=),min)
import Control.Applicative((<*>),pure,(<$>))
import Data.Functor(fmap)
import Text.Show(Show,show)
import Jumpie.List(setPartList,replaceNth)

data Platform = Platform PointInt PointInt deriving(Show)

showPlatforms :: RectInt -> [Platform] -> [String]
showPlatforms r ps' = showPlatforms' (replicate h (replicate w '0')) ps'
  where showPlatforms' :: [String] -> [Platform] -> [String]
        showPlatforms' s ((Platform (Point2 x0 y0) (Point2 x1 _)):ps) = showPlatforms' (replaceNth s y0 (setPartList (s !! y0) (x0,x1) '1')) ps
        showPlatforms' s [] = s
        (Point2 w h) = dimensions r

showPlatformsPpm :: RectInt -> [Platform] -> String
showPlatformsPpm r@(Rect _ (Point2 w h)) ps = unlines $ ["P1",show w ++ " " ++ show h] ++ ((intersperse ' ') <$> (showPlatforms r ps))

validPlatforms :: [Platform] -> [Platform]
validPlatforms ps' = validPlatforms' [] ps'
  where validPlatforms' :: [Platform] -> [Platform] -> [Platform]
        validPlatforms' ns (p:ps) = if intersects ns p || unreachable ns p
                                    then validPlatforms' ns ps
                                    else validPlatforms' (p:ns) ps
        validPlatforms' ns [] = ns
        intersects :: [Platform] -> Platform -> Bool
        intersects ps p = or (pure (pIntersects p) <*> ps)
        unreachable :: [Platform] -> Platform -> Bool
        unreachable ps p = or (pure (pReachable p) <*> ps)

randomPlatform :: RandomGen r => r -> RectInt -> Int -> (Platform,r)
randomPlatform r0 (Rect (Point2 left top) (Point2 right bottom)) maxLength =
  case randomR (left,right) r0 of
    (pleft,r1) -> case randomR (pleft,min right (pleft+maxLength)) r1 of
      -- Fehler liegt hier, das muss nicht ptop sein und unten muss nicht pright hin
      (ptop,r2) -> case randomR (top,bottom) r2 of
        (pright,r3) -> (Platform (Point2 pleft ptop) (Point2 pright ptop),r3)

randomPlatforms :: RandomGen r => r -> RectInt -> Int -> [Platform]
randomPlatforms r0 rect maxLength = (traceShowId p1) : (randomPlatforms r1 rect maxLength)
  where (p1,r1) = randomPlatform r0 rect maxLength

-- Schneiden sich zwei Plattformen
pIntersects :: Platform -> Platform -> Bool
pIntersects (Platform (Point2 l0 t0) (Point2 r0 _)) (Platform (Point2 l1 t1) (Point2 r1 _)) =
  t0 == t1 && (l1 `inside` (l0,r0) || r1 `inside` (l0,r0) || l0 `inside` (l1,r1) || r0 `inside` (l1,r1))

inside :: Ord a => a -> (a,a) -> Bool
inside p (l,r) = p >= l && p <= r

-- Plattform zu Linie
pToLineSegment :: Platform -> LineSegmentReal
pToLineSegment (Platform (Point2 x0 y0) (Point2 x1 _)) = (fmap . fmap) fromIntegral (LineSegment (Point2 x0 y0) (Point2 x1 y0))

both :: (a -> b) -> (a,a) -> (b,b)
both f (a,b) = (f a,f b)

-- Ist Plattform A von Plattform B erreichbar
pReachable :: Platform -> Platform -> Bool
pReachable p0@(Platform (Point2 _ t0) _) p1@(Platform (Point2 _ t1) _) =
  pReachable' `uncurry` (both (pToLineSegment >>> ((fmap . fmap) (*gcTileSize))) (if t0 <= t1 then (p0,p1) else (p1,p0)))

-- Ist Plattform A von Plattform B erreichbar (basiert auf Linien, nicht mehr auf Plattformen)
pReachable' :: LineSegmentReal -> LineSegmentReal -> Bool
pReachable' (LineSegment l0 r0) upper =
  (pParabolaIntersects l0 upper) || (pParabolaIntersects r0 upper)

pParabolaIntersections :: PointReal -> Real -> (PointReal,PointReal)
pParabolaIntersections p@(Point2 _ y0) y = both (pParabola p) ((-1) * term,term)
  where term = sqrt ( (2.0*(y-y0))/gcGrv + (gcJmp*gcJmp)/(gcGrv*gcGrv) ) - gcPlayerMaxSpeed/gcGrv

-- Diese Funktion koennte auf allgemeine Linesegments ausgeweitet
-- werden. Dazu muss nur pParabolaIntersections angepasst werden und hier
-- unten eine ganze Linie uebergeben werden.
pParabolaIntersects :: PointReal -> LineSegmentReal -> Bool
pParabolaIntersects pstart (LineSegment (Point2 lx1 y) (Point2 lx2 _)) = xl `inside` (lx1,lx2) || xr `inside` (lx1,lx2)
  where (Point2 xl _,Point2 xr _) = pParabolaIntersections pstart y

pParabola :: PointReal -> Real -> PointReal
pParabola (Point2 x0 y0) t = Point2 (x0 + gcPlayerMaxSpeed * t) (gcGrv/2.0 * t * t + gcJmp * t + y0)
