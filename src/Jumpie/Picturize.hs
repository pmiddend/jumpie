{-# LANGUAGE OverloadedStrings #-}
module Jumpie.Picturize(
  RenderPositionMode(..),
  picturizeGameState
  ) where

import Data.List((!!))
import           Data.Map.Strict             ((!))
import           Data.Maybe                  (fromJust)
--import           Debug.Trace                 (trace)
import           Control.Monad.State.Strict  (get, gets)

import           Jumpie.GameConfig           (gcStarWiggleHeight,
                                              gcStarWiggleSpeed,screenWidth,screenHeight)
import           Jumpie.GameData             (GameDataM, gdAnims,
                                              gdCurrentTicks, gdSurfaces)
import           Jumpie.GameObject           (Box (Box), BoxType (..),
                                              GameObject (..), Player,
                                              PlayerMode (..),
                                              SensorLine (SensorLine),
                                              Star (..), playerMode,
                                              playerPosition, playerVelocity,
                                              playerWalkSince)
import           Jumpie.GameState            (GameState, gsAllObjects,gsPlayer)
import           Jumpie.Geometry.LineSegment (lineSegmentFrom,lineSegmentTo)
import           Jumpie.Geometry.Rect        (rectTopLeft)
import Control.Lens((^.))
import Wrench.ImageData
import Wrench.Time
import Wrench.Rectangle
import Wrench.Picture
import ClassyPrelude
import Linear.Vector((^*))
import Linear.V2
import Wrench.RenderPositionMode

picturizeGameState :: GameState -> GameDataM p Picture
picturizeGameState gs = do
  picturizedObjects <- traverse picturizeObject (gsAllObjects gs)
  let
    backgroundPicture = pictureSpriteResampled "background" RenderPositionTopLeft (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))
    playerX = playerPosition (gsPlayer gs) ^. _x
  return (backgroundPicture <> V2 (-playerX) 0 `pictureTranslated` pictures picturizedObjects)

picturizeObject :: GameObject -> GameDataM p Picture
picturizeObject ob = case ob of
  ObjectPlayer p -> picturizePlayer p
  ObjectSensorLine s -> picturizeLine s
  ObjectBox b -> picturizeBox b
  ObjectStar s -> picturizeStar s

picturizeBox :: Box -> GameDataM p Picture
picturizeBox (Box p t) = return (rectTopLeft p `pictureTranslated` pictureSpriteTopLeft ("platform" ++ boxTypeToSuffix t))

picturizeStar :: Star -> GameDataM p Picture
picturizeStar (Star pos t) = do
  currentTicks <- gets gdCurrentTicks
  let wiggledPos = pos + V2 0 (gcStarWiggleHeight * sin (toSeconds (currentTicks `tickDelta` t)*gcStarWiggleSpeed))
  return (wiggledPos `pictureTranslated` pictureSpriteCentered "star")

picturizeLine :: SensorLine -> GameDataM p Picture
picturizeLine (SensorLine s) = return (pictureLine (lineSegmentFrom s) (lineSegmentTo s))

boxTypeToSuffix :: IsString s => BoxType -> s
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"

picturizePlayer :: Player -> GameDataM p Picture
picturizePlayer p = do
  ticks <- gdCurrentTicks <$> get
  gd <- get
  let   pp = playerPosition p - (^* 0.5) (playerRect ^. rectangleDimensions)
        playerImage :: Text
        playerImage
          | playerMode p == Air = "player_fly_" ++ playerDirection
          | playerStands || isNothing (playerWalkSince p) = "player_stand"
          | otherwise = playerImageWalk (fromJust (playerWalkSince p))
        playerStands = abs (((^. _x) . playerVelocity) p) <= 0.01
        playerImageWalk walkSince = animFrames playerWalkAnim !! playerImageWalkIndex walkSince
        playerImageWalkIndex walkSince = floor (toSeconds (ticks `tickDelta` walkSince) / (fromIntegral (animFrameSwitch playerWalkAnim) / 1000.0)) `mod` length (animFrames playerWalkAnim)
        playerWalkAnim = gdAnims gd ! ("player_walk_" ++ playerDirection)
        playerDirection = if ((^. _x) . playerVelocity) p <= 0.0 then "left" else "right"
        (_,playerRect) = gdSurfaces gd ! playerImage
  return (pp `pictureTranslated` pictureSpriteTopLeft playerImage)

{-
drawCross :: Surface -> Point2 Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 5 0)) (p + (Point2 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 0 5)) (p + (Point2 0 5))

toDoubleLine :: LineSegment (Point2 Int) -> LineSegment (PointReal)
toDoubleLine = fmap (fmap fromIntegral)
-}
