{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Jumpie.LevelGeneration(
  Platform(Platform),
  pLeft,
  pRight,
  pTiles,
  showPlatforms,
  replaceNth,
  setPartList,
  showPlatformsPpm,
  platformReachable,
  playerMaxJumpHeight,
  playerMaxJumpWidth,
  newLevelGen) where

--import           Control.Category             ((>>>))
import           Control.Monad.Random         (MonadRandom, getRandomR)
import           Jumpie.GameConfig            (gcGrv, gcJmp, gcPlayerMaxSpeed,
                                               gcTileSize, gcTimeMultiplier)
--import           Jumpie.Geometry.Intersection (parabolaPointIntersects)
--import           Jumpie.Geometry.LineSegment  (LineSegment (LineSegment))
import           Jumpie.Geometry.Parabola     (Parabola (Parabola), paraInvert,paraZenith,paraBounds)
import           Jumpie.Geometry.Rect(Rect(..),dimensions)
import Control.Monad.Writer.Class
import           Jumpie.List                  (orEmptyTrue, replaceNth,
                                               setPartList)
--import           Jumpie.Tuple                 (both)
import           Jumpie.Random                 (randomElemM)
import           Jumpie.Types                 (LineSegmentReal, PointInt,
                                               PointReal, Real, RectInt)
import ClassyPrelude hiding(Real,intersect,maximum,(\\))
import Linear.V2
import Data.List((!!),maximum,(\\))
import Control.Lens((^.))

data Platform = Platform PointInt PointInt deriving(Show)

safeSearch s a = if length s <= a then error "Couldn't: \"" <> show a <> "\"" else s !! a

showPlatforms :: RectInt -> [Platform] -> [String]
showPlatforms r = showPlatforms' (replicate h (replicate w '0'))
  where showPlatforms' :: [String] -> [Platform] -> [String]
        showPlatforms' s (Platform (V2  x0 y0) (V2  x1 _):ps) = showPlatforms' (replaceNth s y0 (setPartList (safeSearch s y0{-(s !! y0)-}) (x0,x1+1) '1')) ps
        showPlatforms' s [] = s
        (V2  w h) = dimensions r

showPlatformsPpm :: RectInt -> [Platform] -> String
showPlatformsPpm r@(Rect _ (V2  w h)) ps = unlines $ ["P1",show w ++ " " ++ show h] ++ reverse (intersperse ' ' <$> showPlatforms r ps)

showText :: Show a => a -> Text
showText = pack . show

newLevelGen :: (MonadRandom m,MonadWriter [Text] m) => (Int,Int) -> Int -> [Platform] -> m [Platform]
newLevelGen yrange maxLength startPlatforms =
  case startPlatforms of
    [] -> do
      y <- getRandomR yrange
      let
        x = 0
      p <- randomPlatformAt (V2 x y) maxLength
      tell ["newLevelGen: Initial platform is " <> showText p]
      return [p]
    plats -> do
      tell ["newLevelGen: Noninitial iteration, platforms: " <> showText plats]
      let
        rightmost = maximum (map (\(Platform _ (V2 r _)) -> r) plats)
      tell ["newLevelGen: Rightmost: " <> showText rightmost]
      numberOfPlats <- getRandomR ((2,4) :: (Int,Int))
      tell ["newLevelGen: Number of plats: " <> showText numberOfPlats]
      newPlatforms plats numberOfPlats (rightmost+2,rightmost+4) yrange maxLength

newPlatforms :: (MonadRandom m,MonadWriter [Text] m) => [Platform] -> Int -> (Int,Int) -> (Int,Int) -> Int -> m [Platform]
newPlatforms previous count' (xl,xr) (miny,maxy) maxLength = newPlatforms' count' [V2 x y | x <- [xl..xr], y <- [miny..maxy]]
  where
    newPlatforms' 0 _ = do
      tell ["Pruning because count=0" ]
      return []
    newPlatforms' c [] = do
      tell ["Pruning because no combos left (count " <> showText c <> ")"]
      return []
    newPlatforms' count xyCombos = do
      pos <- randomElemM xyCombos
      tell ["newPlatforms: trying " <> showText pos]
      p <- tryNewPlatform pos previous maxLength
      case p of
        Nothing -> do
          tell ["newPlatforms: didn't work, removing " <> showText pos]
          newPlatforms' count (xyCombos \\ [pos])
        Just p' -> do
          tell ["newPlatforms: new platform works: " <> showText p']
          let
            py = pos ^. _y
            takeOut = [V2 x' y' | x' <- [xl..xr], y' <- [py-1,py,py+1]]
            remaining = xyCombos \\ takeOut
          tell ["newPlatforms: taking out " <> showText takeOut]
          tell ["newPlatforms: remaining combos " <> showText remaining]
          ps <- newPlatforms' (count-1) remaining
          return (p' : ps)

tryNewPlatform :: (MonadRandom m,MonadWriter [Text] m) => V2 Int -> [Platform] -> Int -> m (Maybe Platform)
tryNewPlatform pos previous maxLength = do
  p <- randomPlatformAt pos maxLength
  tell ["tryNewPlatform: generated " <> showText pos]
  if unreachable previous p
    then return Nothing
    else return (Just p)

unreachable :: [Platform] -> Platform -> Bool
unreachable ps p = not $ orEmptyTrue r
  where r = pure (\p' -> platformReachable p' p) <*> ps

randomPlatformAt :: MonadRandom m => PointInt -> Int -> m Platform
randomPlatformAt (V2 x y) maxLength = do
  plength <- getRandomR (1,maxLength)
  return $ Platform (V2 x y) (V2 (x+plength) y)

playerMaxJumpHeight :: Real
playerMaxJumpHeight = paraZenith (playerParabola 1)

playerMaxJumpWidth :: Real
playerMaxJumpWidth =
  let (l,r) = paraBounds (playerParabola 1) 0
  in abs (r - l)

playerParabola :: Real -> Parabola Real
playerParabola fmult = Parabola (-gcGrv/(2*f*f),-gcJmp/f,0)
  where f = fmult * (-1) * (gcPlayerMaxSpeed * gcTimeMultiplier * gcGrv) / gcJmp

pHigher :: Platform -> Platform -> Bool
pHigher p1 p2 = pHeight p1 > pHeight p2

pLowerEqual :: Platform -> Platform -> Bool
pLowerEqual p1 p2 = pHeight p1 <= pHeight p2

platformReachable :: Platform -> Platform -> Bool
platformReachable left right =
  let
    (V2 dx dy) = (fromIntegral . (*gcTileSize)) <$> platformManhattanDistance left right
  in
    dx < playerMaxJumpWidth &&
    (left `pHigher` right) ||
    (left `pLowerEqual` right && dy < playerMaxJumpHeight)

intervalDistance :: (Num a,Ord a) => (a,a) -> (a,a) -> a
intervalDistance (a1,a2) (b1,b2) | a2 < b1 = b1 - a2
                                 | b2 < a1 = a1 - b2
                                 | otherwise = 0

pToXInterval :: Platform -> (Int,Int)
pToXInterval p = (pLeft p ^. _x,pRight p ^. _x)

platformManhattanDistance :: Platform -> Platform -> PointInt
platformManhattanDistance p1 p2 =
  let
    x = intervalDistance (pToXInterval p1) (pToXInterval p2)
    y = abs (pHeight p1 - pHeight p2)
  in V2 x y

{-

validPlatforms :: MonadRandom m => Int -> [Parabola Real] -> m Platform -> m [Platform]
validPlatforms ma paras randPlat = validPlatforms' ma paras randPlat []

validPlatforms' :: MonadRandom m => Int -> [Parabola Real] -> m Platform -> [Platform] -> m [Platform]
validPlatforms' ma paras randPlat ns | length ns == ma = return ns
                                      | otherwise = do
                                        x <- randPlat
                                        if not (intersects ns x) && not (unreachable paras ns x)
                                          then validPlatforms' ma paras randPlat (x:ns)
                                          else validPlatforms' ma paras randPlat ns
  where intersects :: [Platform] -> Platform -> Bool
        intersects ps p = or r
          where r = pure (pIntersects p) <*> ps

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

-}
{-
-- Schneiden sich zwei Plattformen
pIntersects :: Platform -> Platform -> Bool
--pIntersects p0 p1 = trace (show p0 ++ " intersects " ++ show p1 ++ " = " ++ show r) $ r
pIntersects p0 p1 = r
  where r = not $ null $ pAugTiles p0 `intersect` pTiles p1
--  t0 == t1 && (l1 `between` (l0,r0) || r1 `between` (l0,r0) || l0 `between` (l1,r1) || r0 `between` (l1,r1))
-}

pTiles :: Platform -> [PointInt]
pTiles (Platform (V2  l0 t0) (V2  r0 _)) = [V2  x t0 | x <- [l0..r0]]

pLeft :: Platform -> PointInt
pLeft (Platform p _) = p

pHeight :: Platform -> Int
pHeight (Platform (V2 _ y) _) = y

pRight :: Platform -> PointInt
pRight (Platform _ r) = r

{-
pAugTiles :: Platform -> [PointInt]
pAugTiles (Platform (V2  l t) (V2  r _)) = concatMap pTiles $ makePlat <$> [(wl,wr,t-1),(wl,wr,t),(wl,wr,t+1)]
  where wl = l-1
        wr = r+1
        makePlat (l0,r0,y0) = Platform (V2  l0 y0) (V2  r0 y0)

-- Plattform zu Linie
pToLineSegment :: Platform -> LineSegmentReal
pToLineSegment (Platform (V2  x0 y0) (V2  x1 _)) = (fmap . fmap) fromIntegral (LineSegment (V2  x0 y0) (V2  x1 y0))
-}

{-
-- Ist Plattform A von Plattform B erreichbar
pReachable :: [Parabola Real] -> Platform -> Platform -> Bool
pReachable paras p0@(Platform (V2  _ t0) _) p1@(Platform (V2  _ t1) _) =
  pReachable' paras `uncurry` both (pToLineSegment >>> appTS) lowerPair
  where lowerPair = if t0 <= t1 then (p0,p1) else (p1,p0)
        appTS = (fmap . fmap) (*fromIntegral gcTileSize)

-- Ist Plattform A von Plattform B erreichbar (basiert auf Linien, nicht mehr auf Plattformen)
pReachable' :: [Parabola Real] -> LineSegmentReal -> LineSegmentReal -> Bool
pReachable' paras (LineSegment l0 r0) upper = pParabolaIntersects paras l0 upper || pParabolaIntersects paras r0 upper

pParabolaIntersects :: [Parabola Real] -> PointReal -> LineSegmentReal -> Bool
pParabolaIntersects paras p (LineSegment l r) = or $ parabolaPointIntersects <$> paras <*> [l - p,r - p]

easyParabolas :: [Parabola Real]
easyParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0,0.95..0.05]

difficultParabolas :: [Parabola Real]
difficultParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0]

-}
