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
  platformManhattanDistance,
  iterateNewPlatforms,
  newLevelGen) where

--import           Control.Category             ((>>>))
import           Control.Monad.Random         (MonadRandom, getRandomR)
import           Jumpie.GameConfig            (gcGrv, gcJmp, gcPlayerMaxSpeed,
                                               gcTileSize)
--import           Jumpie.Geometry.Intersection (parabolaPointIntersects)
--import           Jumpie.Geometry.LineSegment  (LineSegment (LineSegment))
import           Jumpie.Geometry.Rect(Rect(..),dimensions)
import Control.Monad.Writer.Class
import           Jumpie.List                  (orEmptyTrue, replaceNth,
                                               setPartList)
--import           Jumpie.Tuple                 (both)
import           Jumpie.Random                 (randomElemM)
import           Jumpie.Types                 (PointInt,
                                               Real, RectInt)
import ClassyPrelude hiding(Real,intersect,maximum,(\\))
import Linear.V2
import Data.List((!!),maximum,(\\))
import Control.Lens((^.))

data Platform = Platform PointInt PointInt deriving(Show)

safeSearch :: [String] -> Int -> String
safeSearch s a = if length s <= a then error "Couldn't: \"" <> show a <> "\"" else s !! a

showPlatforms :: RectInt -> [Platform] -> [String]
showPlatforms r = showPlatforms' (replicate h (replicate w '0'))
  where showPlatforms' :: [String] -> [Platform] -> [String]
        showPlatforms' s (Platform (V2  x0 y0) (V2  x1 _):ps) = showPlatforms' (replaceNth s y0 (setPartList (safeSearch s y0{-(s !! y0)-}) (x0,x1+1) '1')) ps
        showPlatforms' s [] = s
        (V2  w h) = r ^. dimensions

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

iterateNewPlatforms :: (MonadRandom m, MonadWriter [Text] m) => Int -> (Int, Int) -> Int -> m [Platform]
iterateNewPlatforms count yrange maxLen = myIterate count (newLevelGen yrange maxLen)

myIterate :: (Semigroup t, Monoid t, Monad m) => Int -> (t -> m t) -> m t
myIterate count g = f' count mempty
  where
    f' 0 _ = return mempty
    f' c xstart = do
      r <- g xstart
      rest <- f' (c-1) r
      return (r <> rest)

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
playerMaxJumpHeight = -(gcJmp*gcJmp)/(2*(-gcGrv))

playerMaxJumpWidth :: Real
playerMaxJumpWidth = (2*(-gcJmp)*gcPlayerMaxSpeed)/gcGrv

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

pTiles :: Platform -> [PointInt]
pTiles (Platform (V2  l0 t0) (V2  r0 _)) = [V2  x t0 | x <- [l0..r0]]

pLeft :: Platform -> PointInt
pLeft (Platform p _) = p

pHeight :: Platform -> Int
pHeight (Platform (V2 _ y) _) = y

pRight :: Platform -> PointInt
pRight (Platform _ r) = r
