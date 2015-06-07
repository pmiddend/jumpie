{-# LANGUAGE TupleSections #-}

module Jumpie.GameGeneration(
    generateGame
  , generateSection
  , moveSection
  , sectionBeginEnd
  , WorldSection
  ) where

import           Jumpie.GameConfig      (gcPlatMaxLength,
                                         gcTileSize, screenHeight, screenWidth)
import           Control.Monad.Writer.Strict       (runWriterT)
import           Jumpie.GameObject
import           Jumpie.Platform
import qualified Jumpie.LevelGeneration as LG
import           Jumpie.Geometry.Rect
import           Jumpie.Types
--import Jumpie.Debug(traceShowId)
import           Control.Monad.Random   (MonadRandom)
import ClassyPrelude hiding(head,minimum,maximum,Real)
import Linear.V2
import Wrench.Time
import Control.Lens((^.),view)
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
tilesPerScreen :: V2 Int
tilesPerScreen = V2 (screenWidth `div` gcTileSize) (screenHeight `div` gcTileSize)

--tilesRect :: RectInt
--tilesRect = Rect (V2 1 1) (V2 (screenWidth `div` gcTileSize - 1) (screenHeight `div` gcTileSize - 1))

abovePlatPosition :: Platform -> PointReal
abovePlatPosition p = V2 (p ^. platRectAbsReal . center . _x) (p ^. platRectAbsReal . rectTopLeft . _y)

{-
randomStar :: MonadRandom m => TimeTicks -> [GameObject] -> m Star
randomStar ticks xs = do
  p <- randomAbovePlatPosition xs
  return $ Star (p - V2 0 (fromIntegral (gcTileSize `div` 2))) ticks
-}

type WorldSection = [GameObject]

generateSection :: MonadRandom m => TimeTicks -> [Platform] -> m WorldSection
generateSection timeTicks prevPlats = do
  let
    platsAction = LG.iterateNewPlatforms 1 (1,tilesPerScreen ^. _y) gcPlatMaxLength timeTicks prevPlats 
  (plats,_) <- runWriterT platsAction
  let
    boxes = ObjectPlatform <$> plats
  return boxes

sectionBeginEnd :: WorldSection -> (TileIncrement,TileIncrement)
sectionBeginEnd objects =
  let boxes = mapMaybe maybePlatform objects
      minPos = minimum (view platLeft <$> boxes)
      maxPos = maximum (view platRight <$> boxes)
  in (TileIncrement minPos,TileIncrement maxPos)

moveSection :: TileIncrement -> WorldSection -> WorldSection
moveSection r s = (`moveObject` r) <$> s

generateGame :: MonadRandom m => TimeTicks -> m (Player,[WorldSection])
generateGame currentTicks = do
  section <- generateSection currentTicks []
  let
    firstBox = head (sortBy (comparing (view platLeft)) (mapMaybe maybePlatform section))
    rawPlayerPos = abovePlatPosition firstBox
    playerPos = rawPlayerPos + V2 (fromIntegral gcTileSize / 2) (fromIntegral (-gcTileSize))
  let player = Player {
        _playerPosition = playerPos,
        _playerMode = Air,
        _playerVelocity = V2 0.0 0.0,
        _playerWalkSince = Nothing
      }
  return (player,[section])
