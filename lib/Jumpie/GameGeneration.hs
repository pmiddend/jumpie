{-# LANGUAGE TupleSections #-}

module Jumpie.GameGeneration(
    generateGame
  , generateSection
  , moveSection
  , sectionWidth
  , WorldSection
  ) where

import           Jumpie.GameConfig      (gcPlatMaxLength,
                                         gcTileSize, screenHeight, screenWidth)
import           Control.Monad.Writer.Strict       (runWriterT)
import           Jumpie.GameObject
import qualified Jumpie.LevelGeneration as LG
import           Jumpie.Geometry.Rect
import           Jumpie.Types
--import Jumpie.Debug(traceShowId)
import           Control.Monad.Random   (MonadRandom)
import ClassyPrelude hiding(head,minimum,maximum,Real)
import           Jumpie.LevelGeneration (Platform (Platform), pTiles)
import Linear.V2
import Wrench.Time
import Control.Lens((^.),view,(&),(+~))
import Data.List(head,minimum,maximum)

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

--tilesRect :: RectInt
--tilesRect = Rect (V2 1 1) (V2 (screenWidth `div` gcTileSize - 1) (screenHeight `div` gcTileSize - 1))

platToBoxes :: TimeTicks -> [PointInt] -> Platform -> [Box]
platToBoxes currentTicks plats (Platform (V2 l y) (V2 r _)) = map toBox [l..r]
  where toBox x = Box (bpos x) (currentTicks `plusDuration` (fromSeconds 10)) (btype x)
        bpos x = (fmap . fmap) (fromIntegral . (*gcTileSize)) (Rect (V2 x y) (V2 (x+1) (y+1)))
        btype x
          | hasLeft && hasRight = BoxMiddle
          | hasLeft = BoxLeft
          | hasRight = BoxRight
          | otherwise = BoxSingleton
          where hasLeft = V2 (x - 1) y `elem` plats
                hasRight = V2 (x + 1) y `elem` plats

abovePlatPosition :: Box -> PointReal
abovePlatPosition b = (b ^. boxPosition . rectTopLeft) & _x +~ (fromIntegral gcTileSize / 2)

{-
randomStar :: MonadRandom m => TimeTicks -> [GameObject] -> m Star
randomStar ticks xs = do
  p <- randomAbovePlatPosition xs
  return $ Star (p - V2 0 (fromIntegral (gcTileSize `div` 2))) ticks
-}

type WorldSection = [GameObject]

generateSection :: MonadRandom m => TimeTicks -> m WorldSection
generateSection timeTicks = do
  let
    platsAction = LG.iterateNewPlatforms 10 (0,tilesPerScreen ^. _y) gcPlatMaxLength
  (plats,_) <- runWriterT platsAction
  let
    platformPoints = platsToPoints plats
    boxes = ObjectBox <$> concatMap (platToBoxes timeTicks platformPoints) plats
  return boxes

sectionWidth :: WorldSection -> (Real,Real)
sectionWidth objects =
  let boxes = view boxPosition <$> (mapMaybe maybeBox objects)
      minPos = minimum (view (rectTopLeft . _x) <$> boxes)
      maxPos = maximum (view (rectBottomRight . _x) <$> boxes)
  in (minPos,maxPos)

moveSection :: Real -> WorldSection -> WorldSection
moveSection r s = (\o -> moveObject o (V2 r 0)) <$> s

generateGame :: MonadRandom m => TimeTicks -> m (Player,[WorldSection])
generateGame currentTicks = do
  section <- generateSection currentTicks
  let
    firstBox = head (sortBy (comparing (view (boxPosition . rectTopLeft . _x))) (mapMaybe maybeBox section))
    rawPlayerPos = abovePlatPosition firstBox
    playerPos = rawPlayerPos + V2 (fromIntegral gcTileSize / 2) (fromIntegral (-gcTileSize))
  let player = Player {
        _playerPosition = playerPos,
        _playerMode = Air,
        _playerVelocity = V2 0.0 0.0,
        _playerWalkSince = Nothing
      }
  secondSection <- generateSection (currentTicks `plusDuration` fromSeconds 20)
  let (_,firstSectionEnd) = sectionWidth section
  return (player,[section,(moveSection (firstSectionEnd + (fromIntegral gcTileSize)) secondSection)])
