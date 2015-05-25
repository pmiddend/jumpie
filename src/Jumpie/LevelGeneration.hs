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
  newLevelGen,
  randomPlatforms) where

import           Control.Category             ((>>>))
import           Control.Monad.Random         (MonadRandom, getRandomR)
import           Jumpie.GameConfig            (gcGrv, gcJmp, gcPlayerMaxSpeed,
                                               gcTileSize, gcTimeMultiplier)
import           Jumpie.Geometry.Intersection (parabolaPointIntersects)
import           Jumpie.Geometry.LineSegment  (LineSegment (LineSegment))
import           Jumpie.Geometry.Parabola     (Parabola (Parabola), paraInvert)
import           Jumpie.Geometry.Rect(Rect(..),dimensions)
import           Jumpie.List                  (orEmptyTrue, replaceNth,
                                               setPartList)
import           Jumpie.Tuple                 (both)
import           Jumpie.Random                 (randomElemM)
import           Jumpie.Types                 (LineSegmentReal, PointInt,
                                               PointReal, Real, RectInt)
import ClassyPrelude hiding(Real,intersect,maximum,(\\))
import Linear.V2
import Data.List((!!),intersect,maximum,(\\))
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

newPlatform :: MonadRandom m => Int -> Int -> [Platform] -> Int -> m (Maybe Platform)
newPlatform x y previous maxLength = do
  p <- randomPlatformFixed (V2 x y) maxLength
  if unreachable easyParabolas previous p
    then return Nothing
    else return (Just p)

newPlatforms :: MonadRandom m => [Platform] -> Int -> (Int,Int) -> Int -> Int -> m [Platform]
newPlatforms previous count' (xl,xr) maxy maxLength = newPlatforms' count' (myTrace ("Initial combos" <> )[V2 x y | x <- [xl..xr], y <- [0..maxy]])
  where
    newPlatforms' 0 _ = return (trace "Pruning because count=0" [])
    newPlatforms' c [] = return (trace ("Pruning because no combos left (count " <> show c <> ")") [])
    newPlatforms' count xyCombos = do
      (V2 x y) <- randomElemM xyCombos
      p <- newPlatform x y previous maxLength
      case p of
        Nothing -> newPlatforms' count (xyCombos \\ [V2 x y])
        Just p' -> do
          let takeOut = myTrace' (\f -> "take out " <> show f <> ", remaining: " <> show (xyCombos \\ f)) [V2 x' y' | x' <- [xl..xr], y' <- [y-1,y,y+1]]
          ps <- newPlatforms' (count-1) (myTrace' (\x''-> "combos remaining: " <> show (length x'')) $! (xyCombos \\ takeOut))
          return ((myTrace (\x -> "Using " <> x) p') : ps)

myTrace f a = trace (f (show a)) a
myTrace' f a = trace (f a) a

newLevelGen :: MonadRandom m => RectInt -> Int -> Maybe [Platform] -> m [Platform]
newLevelGen boundingBox maxLength startPlatforms =
  case startPlatforms of
    Nothing -> do
      let bottom = rectBottomRight boundingBox ^. _y - 1
      p <- randomPlatform (Rect (V2 0 0) (V2 maxLength (bottom - bottom `div` 2))) maxLength
      return [myTrace ("Using: " <> ) p]
    Just plats -> do
      let
        rightmost = maximum (map (\(Platform _ (V2 r _)) -> r) plats)
      numberOfPlats <- getRandomR ((2,4) :: (Int,Int))
      newPlatforms plats (myTrace ("numberOfPlats: " <>) numberOfPlats) (rightmost+2,rightmost+4) (rectBottomRight boundingBox ^. _y) maxLength

validPlatforms :: MonadRandom m => Int -> [Parabola Real] -> m Platform -> m [Platform]
validPlatforms ma paras randPlat = validPlatforms' ma paras randPlat []

unreachable :: [Parabola Real] -> [Platform] -> Platform -> Bool
unreachable paras ps p = not $ orEmptyTrue r
  where r = pure (\p' -> pReachable paras p' p) <*> ps

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

randomPlatformFixed :: MonadRandom m => PointInt -> Int -> m Platform
randomPlatformFixed (V2 x y) maxLength = do
  plength <- getRandomR (1,maxLength)
  return $ Platform (V2 x y) (V2 (x+plength) y)

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
  pReachable' paras `uncurry` both (pToLineSegment >>> appTS) lowerPair
  where lowerPair = if t0 <= t1 then (p0,p1) else (p1,p0)
        appTS = (fmap . fmap) (*fromIntegral gcTileSize)

-- Ist Plattform A von Plattform B erreichbar (basiert auf Linien, nicht mehr auf Plattformen)
pReachable' :: [Parabola Real] -> LineSegmentReal -> LineSegmentReal -> Bool
pReachable' paras (LineSegment l0 r0) upper = pParabolaIntersects paras l0 upper || pParabolaIntersects paras r0 upper

pParabolaIntersects :: [Parabola Real] -> PointReal -> LineSegmentReal -> Bool
pParabolaIntersects paras p (LineSegment l r) = or $ parabolaPointIntersects <$> paras <*> [l - p,r - p]

easyParabolas :: [Parabola Real]
easyParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0,0.75,0.5,0.25,0.125]

difficultParabolas :: [Parabola Real]
difficultParabolas = concatMap (\f -> [playerParabola f,paraInvert (playerParabola f)]) [1.0]

playerParabola :: Real -> Parabola Real
playerParabola fmult = Parabola (-gcGrv/(2*f*f),-gcJmp/f,0)
  where f = fmult * (-1) * (gcPlayerMaxSpeed * gcTimeMultiplier * gcGrv) / gcJmp
