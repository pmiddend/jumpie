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
import Jumpie.Tuple(both)
import Jumpie.Geometry.Rect(Rect(Rect),dimensions)
import Jumpie.Geometry.Intersection(parabolaPointIntersects)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Jumpie.Geometry.Parabola(Parabola(Parabola),paraInvert)
import Jumpie.Types(RectInt,PointInt,LineSegmentReal,PointReal,Real)
import Jumpie.GameConfig(gcGrv,gcTimeMultiplier,gcJmp,gcPlayerMaxSpeed,gcTileSize)
import Data.Bool((||),Bool,(&&),not)
import Control.Category((>>>))
import Data.Function((.),($))
import Data.Tuple(uncurry)
import Prelude((*),sqrt,(-),(/),(+),fromIntegral,otherwise)
import Data.List(or,(!!),(++),replicate,intersperse,unlines,concatMap,intersect,null,reverse)
import Data.Int(Int)
import Data.String(String)
import Data.Ord(Ord,(<=),min)
import Control.Applicative((<*>),pure,(<$>))
import Data.Functor(fmap)
import Text.Show(Show,show)
import Jumpie.List(setPartList,replaceNth,orEmptyTrue,inductiveFilter)

data Platform = Platform PointInt PointInt deriving(Show)

showPlatforms :: RectInt -> [Platform] -> [String]
showPlatforms r ps' = showPlatforms' (replicate h (replicate w '0')) ps'
  where showPlatforms' :: [String] -> [Platform] -> [String]
        showPlatforms' s ((Platform (Point2 x0 y0) (Point2 x1 _)):ps) = showPlatforms' (replaceNth s y0 (setPartList (s !! y0) (x0,x1+1) '1')) ps
        showPlatforms' s [] = s
        (Point2 w h) = dimensions r

showPlatformsPpm :: RectInt -> [Platform] -> String
showPlatformsPpm r@(Rect _ (Point2 w h)) ps = unlines $ ["P1",show w ++ " " ++ show h] ++ (reverse $ (intersperse ' ') <$> (showPlatforms r ps))

validPlatforms :: [Platform] -> [Platform]
validPlatforms = inductiveFilter (\x xs -> not (intersects xs x) && not (unreachable xs x))
  where intersects :: [Platform] -> Platform -> Bool
        --intersects ps p = trace ("intersects: " ++ show r) $ or r
        intersects ps p = or r
          where r = pure (pIntersects p) <*> ps
        -- Obacht, or ist nicht gleich foldr (||) True sondern foldr (||) False
        unreachable :: [Platform] -> Platform -> Bool
        --unreachable ps p = trace ("unreachable " ++ show p ++ ": " ++ show r) $ not $ orEmptyTrue $ r
        unreachable ps p = not $ orEmptyTrue $ r
          where r = pure (pReachable p) <*> ps

randomPlatform :: RandomGen r => r -> RectInt -> Int -> (Platform,r)
randomPlatform r0 (Rect (Point2 left top) (Point2 right bottom)) maxLength =
  case randomR (left,right) r0 of
    (pleft,r1) -> case randomR (pleft,min right (pleft+maxLength)) r1 of
      -- Fehler liegt hier, das muss nicht ptop sein und unten muss nicht pright hin
      (pright,r2) -> case randomR (top,bottom) r2 of
        (ptop,r3) -> (Platform (Point2 pleft ptop) (Point2 pright ptop),r3)

randomPlatforms :: RandomGen r => r -> RectInt -> Int -> [Platform]
randomPlatforms r0 rect maxLength = p1 : (randomPlatforms r1 rect maxLength)
  where (p1,r1) = randomPlatform r0 rect maxLength

-- Schneiden sich zwei Plattformen
pIntersects :: Platform -> Platform -> Bool
--pIntersects p0 p1 = trace (show p0 ++ " intersects " ++ show p1 ++ " = " ++ show r) $ r
pIntersects p0 p1 = r
  where r = not $ null $ pAugTiles p0 `intersect` pTiles p1
--  t0 == t1 && (l1 `between` (l0,r0) || r1 `between` (l0,r0) || l0 `between` (l1,r1) || r0 `between` (l1,r1))

pTiles :: Platform -> [PointInt]
pTiles (Platform (Point2 l0 t0) (Point2 r0 _)) = [Point2 x t0 | x <- [l0..r0]]

pAugTiles :: Platform -> [PointInt]
pAugTiles (Platform (Point2 l t) (Point2 r _)) = concatMap pTiles $ makePlat <$> [(wl,wr,t-1),(wl,wr,t),(wl,wr,t+1)]
  where wl = l-1
        wr = r+1
        makePlat (l0,r0,y0) = Platform (Point2 l0 y0) (Point2 r0 y0)

-- Plattform zu Linie
pToLineSegment :: Platform -> LineSegmentReal
pToLineSegment (Platform (Point2 x0 y0) (Point2 x1 _)) = (fmap . fmap) fromIntegral (LineSegment (Point2 x0 y0) (Point2 x1 y0))

-- Ist Plattform A von Plattform B erreichbar
pReachable :: Platform -> Platform -> Bool
pReachable p0@(Platform (Point2 _ t0) _) p1@(Platform (Point2 _ t1) _) =
  pReachable' `uncurry` (both (pToLineSegment >>> appTS) lowerPair)
  where lowerPair = if t0 <= t1 then (p0,p1) else (p1,p0)
        appTS = (fmap . fmap) (*gcTileSize)

-- Ist Plattform A von Plattform B erreichbar (basiert auf Linien, nicht mehr auf Plattformen)
pReachable' :: LineSegmentReal -> LineSegmentReal -> Bool
pReachable' (LineSegment l0 r0) upper = pParabolaIntersects l0 upper || pParabolaIntersects r0 upper

pParabolaIntersects :: PointReal -> LineSegmentReal -> Bool
pParabolaIntersects p (LineSegment l r) = or $ parabolaPointIntersects <$> testParabolas <*> [l - p,r - p]

--testParabolas :: [Parabola Real]
--testParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0,0.5,0.25]
testParabolas :: [Parabola Real]
testParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0]

playerParabola :: Real -> Parabola Real
playerParabola fmult = Parabola (-gcGrv/(2*f*f),-gcJmp/f,0)
  where f = fmult * (-1) * (gcPlayerMaxSpeed * gcTimeMultiplier * gcGrv) / gcJmp
