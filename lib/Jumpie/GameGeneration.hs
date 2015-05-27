{-# LANGUAGE TupleSections #-}

module Jumpie.GameGeneration(
  generateGame,
  randomAbovePlatPosition
  ) where

import           Jumpie.GameConfig      (gcPlatCount, gcPlatMaxLength,
                                         gcTileSize, screenHeight, screenWidth)
import           Control.Monad.Writer.Strict       (runWriterT)
import           Control.Monad.Random       (evalRandT)
import           Jumpie.GameObject
import qualified Jumpie.LevelGeneration as LG
import           Jumpie.Geometry.Rect   (Rect (Rect), rectTopLeft)
import           Jumpie.Random          (randomElemM)
import           Jumpie.Types           (PointInt, PointReal, RectInt)
--import Jumpie.Debug(traceShowId)
import           Control.Monad.Random   (MonadRandom)
import ClassyPrelude hiding(head)
import           Jumpie.LevelGeneration (Platform (Platform), pTiles)
import Wrench.Time
import Linear.V2
import Control.Lens((^.))
import Data.List(head)

  {-
initialPlayer :: Player
initialPlayer = Player {
  playerPosition = Point2 (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 4.0),
  playerMode = Air,
  playerVelocity = Point2 0.0 0.0,
  playerWalkSince = Nothing
  }

initialBoxes :: [Box]
initialBoxes = map (\x -> toBox x yBaseline) [0..boxesPerScreen-1] ++ otherBoxes
  where rectSize = 35
        yBaseline = fromIntegral $ screenHeight `div` 2 - rectSize `div` 2
        boxesPerScreen = screenWidth `div` rectSize
        otherBoxes = [toBox 2 (yBaseline - fromIntegral rectSize),toBox 10 (yBaseline - 3.0 * fromIntegral rectSize)]
        toBox xRaw yRaw = Box $ Rect {
          rectTopLeft = Point2 (fromIntegral (xRaw*rectSize)) yRaw,
          rectBottomRight = Point2 (fromIntegral ((xRaw+1)*rectSize)) (yRaw + fromIntegral rectSize)
          }
-}
platsToPoints :: [Platform] -> [PointInt]
platsToPoints = concatMap pTiles

tilesPerScreen :: V2 Int
tilesPerScreen = V2 (screenWidth `div` gcTileSize) (screenHeight `div` gcTileSize)

tilesRect :: RectInt
tilesRect = Rect (V2 1 1) (V2 (screenWidth `div` gcTileSize - 1) (screenHeight `div` gcTileSize - 1))

platToBoxes :: [PointInt] -> Platform -> [Box]
platToBoxes plats (Platform (V2 l y) (V2 r _)) = map toBox [l..r]
  where toBox x = Box (bpos x) (btype x)
        bpos x = (fmap . fmap) (fromIntegral . (*gcTileSize)) (Rect (V2 x y) (V2 (x+1) (y+1)))
        btype x
          | hasLeft && hasRight = BoxMiddle
          | hasLeft = BoxLeft
          | hasRight = BoxRight
          | otherwise = BoxSingleton
          where hasLeft = V2 (x - 1) y `elem` plats
                hasRight = V2 (x + 1) y `elem` plats

abovePlatPosition :: Box -> PointReal
abovePlatPosition = (+ (V2 (fromIntegral gcTileSize / 2) 0)) . rectTopLeft . boxPosition

randomAbovePlatPosition :: MonadRandom m => [GameObject] -> m PointReal
randomAbovePlatPosition xs = do
  randomBox <- randomElemM (mapMaybe maybeBox xs)
  return (abovePlatPosition randomBox)

{-
randomStar :: MonadRandom m => TimeTicks -> [GameObject] -> m Star
randomStar ticks xs = do
  p <- randomAbovePlatPosition xs
  return $ Star (p - V2 0 (fromIntegral (gcTileSize `div` 2))) ticks
-}

generateGame :: MonadRandom m => m (Player,[GameObject])
generateGame = do
  let
    platsAction = LG.iterateNewPlatforms 10 (0,tilesPerScreen ^. _y) gcPlatMaxLength
  (plats,_) <- runWriterT platsAction
  let
    platformPoints = platsToPoints plats
    boxes = ObjectBox <$> concatMap (platToBoxes platformPoints) plats
    firstBox = head (sortBy (comparing ((^. _x). rectTopLeft . boxPosition)) (mapMaybe maybeBox boxes))
    rawPlayerPos = abovePlatPosition firstBox
    playerPos = rawPlayerPos + V2 (fromIntegral gcTileSize / 2) (fromIntegral (-gcTileSize))
  let player = Player {
        playerPosition = playerPos,
        playerMode = Air,
        playerVelocity = V2 0.0 0.0,
        playerWalkSince = Nothing
      }
  return (player,boxes)
