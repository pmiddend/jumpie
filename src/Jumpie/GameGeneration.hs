{-# LANGUAGE TupleSections #-}

module Jumpie.GameGeneration(
  generateGame,
  randomAbovePlatPosition
  ) where

import           Jumpie.GameConfig      (gcPlatCount, gcPlatMaxLength,
                                         gcTileSize, screenHeight, screenWidth)
import           Jumpie.GameObject      (Box (Box), BoxType (..),
                                         GameObject (..), Player (Player),
                                         PlayerMode (..), boxPosition, maybeBox,
                                         playerMode, playerPosition,
                                         playerVelocity, playerWalkSince)
import           Jumpie.Geometry.Point  (Point2 (Point2))
import           Jumpie.Geometry.Rect   (Rect (Rect), rectTopLeft)
import           Jumpie.Random          (randomElemM)
import           Jumpie.Types           (PointInt, PointReal, RectInt)
--import Jumpie.Debug(traceShowId)
import           Control.Applicative    ((<$>))
import           Data.Bool              ((&&))
import           Data.Function          (($), (.))
import           Data.Functor           (fmap)
import           Data.List              (concatMap, elem, map, take)
import           Data.Maybe             (Maybe (Nothing), maybeToList)
import           Jumpie.LevelGeneration (Platform (Platform), easyParabolas,
                                         pTiles, randomPlatforms,
                                         validPlatforms)
import           Prelude                (div, fromIntegral, (*), (+), (-), (/))
import Control.Monad.Random(MonadRandom)
import Control.Monad(return)

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
platsToPoints ps = concatMap pTiles ps

tilesRect :: RectInt
tilesRect = (Rect (Point2 1 1) (Point2 (screenWidth `div` gcTileSize - 1) (screenHeight `div` gcTileSize - 1)))

generateRandomPlats :: MonadRandom m => m [Platform]
generateRandomPlats = randomPlatforms tilesRect gcPlatMaxLength

platToBoxes :: [PointInt] -> Platform -> [Box]
platToBoxes plats (Platform (Point2 l y) (Point2 r _)) = map toBox [l..r]
  where toBox x = Box (bpos x) (btype x)
        bpos x = (fmap . fmap) (fromIntegral . (*gcTileSize)) (Rect (Point2 x y) (Point2 (x+1) (y+1)))
        btype x = if hasLeft && hasRight
                  then BoxMiddle
                  else
                    if hasLeft
                    then BoxLeft
                    else
                      if hasRight
                      then BoxRight
                      else BoxSingleton
          where hasLeft = (Point2 (x-1) y) `elem` plats
                hasRight = (Point2 (x+1) y) `elem` plats

randomAbovePlatPosition :: MonadRandom m => [GameObject] -> m PointReal
randomAbovePlatPosition xs = do
  randomBox <- randomElemM $ concatMap (maybeToList . maybeBox) xs
  return . rectTopLeft . boxPosition $ randomBox

generateGame :: MonadRandom m => m [GameObject]
generateGame = do
  rawPlats <- generateRandomPlats
  let plats = take gcPlatCount $ validPlatforms easyParabolas rawPlats
      platformPoints = platsToPoints plats
      boxes = ObjectBox <$> concatMap (platToBoxes platformPoints) plats
  rawPlayerPos <- randomAbovePlatPosition boxes
  let player = Player {
        playerPosition = rawPlayerPos - (Point2 0 (fromIntegral gcTileSize)),
        playerMode = Air,
        playerVelocity = Point2 0.0 0.0,
        playerWalkSince = Nothing
      }
  return $ (ObjectPlayer $ player) : boxes
