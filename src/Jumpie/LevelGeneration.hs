module Jumpie.LevelGeneration(
  Platform(Platform),
  pLeft,
  pRight,
  validPlatforms,
  pTiles,
  difficultParabolas,
  easyParabolas,
  randomPlatform,
  showPlatforms,
  replaceNth,
  setPartList,
  showPlatformsPpm,
  randomPlatforms) where

import           Control.Category             ((>>>))
import           Control.Monad.Random         (MonadRandom, getRandomR)
import           Jumpie.GameConfig            (gcGrv, gcJmp, gcPlayerMaxSpeed,
                                               gcTileSize, gcTimeMultiplier)
import           Jumpie.Geometry.Intersection (parabolaPointIntersects)
import           Jumpie.Geometry.LineSegment  (LineSegment (LineSegment))
import           Jumpie.Geometry.Parabola     (Parabola (Parabola), paraInvert)
import           Jumpie.Geometry.Rect         (Rect (Rect), dimensions)
import           Jumpie.List                  (orEmptyTrue, replaceNth,
                                               setPartList)
import           Jumpie.Tuple                 (both)
import           Jumpie.Types                 (LineSegmentReal, PointInt,
                                               PointReal, Real, RectInt)
import ClassyPrelude hiding(Real,intersect)
import Linear.V2
import Data.List((!!),intersect)

data Platform = Platform PointInt PointInt deriving(Show)

showPlatforms :: RectInt -> [Platform] -> [String]
showPlatforms r ps' = showPlatforms' (replicate h (replicate w '0')) ps'
  where showPlatforms' :: [String] -> [Platform] -> [String]
        showPlatforms' s ((Platform (V2  x0 y0) (V2  x1 _)):ps) = showPlatforms' (replaceNth s y0 (setPartList (s !! y0) (x0,x1+1) '1')) ps
        showPlatforms' s [] = s
        (V2  w h) = dimensions r

showPlatformsPpm :: RectInt -> [Platform] -> String
showPlatformsPpm r@(Rect _ (V2  w h)) ps = unlines $ ["P1",show w ++ " " ++ show h] ++ (reverse $ (intersperse ' ') <$> (showPlatforms r ps))

validPlatforms :: MonadRandom m => Int -> [Parabola Real] -> m Platform -> m [Platform]
validPlatforms ma paras randPlat = validPlatforms' ma paras randPlat []

validPlatforms' :: MonadRandom m => Int -> [Parabola Real] -> m Platform -> [Platform] -> m [Platform]
validPlatforms' ma paras randPlat ns | length ns == ma = return ns
                                      | otherwise = do
                                        x <- randPlat
                                        if not (intersects ns x) && not (unreachable ns x)
                                          then validPlatforms' ma paras randPlat (x:ns)
                                          else validPlatforms' ma paras randPlat ns
  where intersects :: [Platform] -> Platform -> Bool
        intersects ps p = or r
          where r = pure (pIntersects p) <*> ps
        unreachable :: [Platform] -> Platform -> Bool
        unreachable ps p = not $ orEmptyTrue $ r
          where r = pure (pReachable paras p) <*> ps

randomPlatform :: MonadRandom m => RectInt -> Int -> m Platform
randomPlatform (Rect (V2  left top) (V2  right bottom)) maxLength = do
  pleft <- getRandomR (left,right)
  pright <- getRandomR (pleft,min right (pleft+maxLength))
  ptop <- getRandomR (top,bottom)
  return $ Platform (V2 pleft ptop) (V2 pright ptop)

-- Neu
randomPlatforms :: MonadRandom m => RectInt -> Int -> m [Platform]
randomPlatforms rect maxLength = do
  p <- randomPlatform rect maxLength
  ps <- randomPlatforms rect maxLength
  return $ p : ps

-- Schneiden sich zwei Plattformen
pIntersects :: Platform -> Platform -> Bool
--pIntersects p0 p1 = trace (show p0 ++ " intersects " ++ show p1 ++ " = " ++ show r) $ r
pIntersects p0 p1 = r
  where r = not $ null $ pAugTiles p0 `intersect` pTiles p1
--  t0 == t1 && (l1 `between` (l0,r0) || r1 `between` (l0,r0) || l0 `between` (l1,r1) || r0 `between` (l1,r1))

pTiles :: Platform -> [PointInt]
pTiles (Platform (V2  l0 t0) (V2  r0 _)) = [V2  x t0 | x <- [l0..r0]]

pLeft :: Platform -> PointInt
pLeft (Platform p _) = p

pRight :: Platform -> PointInt
pRight (Platform _ r) = r

pAugTiles :: Platform -> [PointInt]
pAugTiles (Platform (V2  l t) (V2  r _)) = concatMap pTiles $ makePlat <$> [(wl,wr,t-1),(wl,wr,t),(wl,wr,t+1)]
  where wl = l-1
        wr = r+1
        makePlat (l0,r0,y0) = Platform (V2  l0 y0) (V2  r0 y0)

-- Plattform zu Linie
pToLineSegment :: Platform -> LineSegmentReal
pToLineSegment (Platform (V2  x0 y0) (V2  x1 _)) = (fmap . fmap) fromIntegral (LineSegment (V2  x0 y0) (V2  x1 y0))

-- Ist Plattform A von Plattform B erreichbar
pReachable :: [Parabola Real] -> Platform -> Platform -> Bool
pReachable paras p0@(Platform (V2  _ t0) _) p1@(Platform (V2  _ t1) _) =
  pReachable' paras `uncurry` (both (pToLineSegment >>> appTS) lowerPair)
  where lowerPair = if t0 <= t1 then (p0,p1) else (p1,p0)
        appTS = (fmap . fmap) (*(fromIntegral gcTileSize))

-- Ist Plattform A von Plattform B erreichbar (basiert auf Linien, nicht mehr auf Plattformen)
pReachable' :: [Parabola Real] -> LineSegmentReal -> LineSegmentReal -> Bool
pReachable' paras (LineSegment l0 r0) upper = pParabolaIntersects paras l0 upper || pParabolaIntersects paras r0 upper

pParabolaIntersects :: [Parabola Real] -> PointReal -> LineSegmentReal -> Bool
pParabolaIntersects paras p (LineSegment l r) = or $ parabolaPointIntersects <$> paras <*> [l - p,r - p]

easyParabolas :: [Parabola Real]
easyParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0,0.5,0.25]

difficultParabolas :: [Parabola Real]
difficultParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0]

playerParabola :: Real -> Parabola Real
playerParabola fmult = Parabola (-gcGrv/(2*f*f),-gcJmp/f,0)
  where f = fmult * (-1) * (gcPlayerMaxSpeed * gcTimeMultiplier * gcGrv) / gcJmp
