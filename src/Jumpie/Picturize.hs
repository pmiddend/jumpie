{-# LANGUAGE OverloadedStrings #-}
module Jumpie.Picturize(
  RenderPositionMode(..),
  picturizeGameState
  ) where

import Data.List((!!))
import           Data.Maybe                  (fromJust)
import           Control.Monad.State.Strict  (get)

import           Jumpie.GameConfig           (screenWidth,screenHeight)
import           Jumpie.GameData
import           Jumpie.GameObject
import           Jumpie.GameState
import           Jumpie.Geometry.LineSegment (lineSegmentFrom,lineSegmentTo)
import           Jumpie.Geometry.Rect        (rectTopLeft)
import Control.Lens((^.))
import Wrench.ImageData
import Wrench.SpriteIdentifier
import Wrench.Animation
import Wrench.Time
import Wrench.Picture
import Wrench.Platform(Platform)
import ClassyPrelude
import Linear.V2
import Wrench.RenderPositionMode

picturizeGameState :: Platform p => GameState -> GameDataM p Picture
picturizeGameState gs = do
  picturizedObjects <- traverse picturizeObject (gsAllObjects gs)
  let
    backgroundPicture = pictureSpriteResampled "background" RenderPositionTopLeft (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))
  return (backgroundPicture <> (-(gsCameraPosition gs)) `pictureTranslated` pictures picturizedObjects)

picturizeObject :: Platform p => GameObject -> GameDataM p Picture
picturizeObject ob = case ob of
  ObjectPlayer p -> picturizePlayer p
  ObjectSensorLine s -> picturizeLine s
  ObjectBox b -> picturizeBox b
  ObjectParticle s -> picturizeParticle s

picturizeBox :: Box -> GameDataM p Picture
picturizeBox (Box p t) = return (rectTopLeft p `pictureTranslated` pictureSpriteTopLeft ("platform" ++ boxTypeToSuffix t))

picturizeParticle :: Particle -> GameDataM p Picture
picturizeParticle (Particle identifier pos inception) = do
  gd <- get
  let
    ticks = gdCurrentTicks gd
    image = currentAnimFrame inception ticks anim
    anim = lookupAnimSafe gd identifier
  return (pos `pictureTranslated` pictureSpriteCentered image)

picturizeLine :: SensorLine -> GameDataM p Picture
picturizeLine (SensorLine s) = return (pictureLine (lineSegmentFrom s) (lineSegmentTo s))

boxTypeToSuffix :: IsString s => BoxType -> s
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"

currentAnimFrame :: TimeTicks -> TimeTicks -> Animation -> SpriteIdentifier
currentAnimFrame animStart currentTicks anim =
  let
    tdelta = toSeconds (currentTicks `tickDelta` animStart)
    noFrames = length (animFrames anim)
    animIndex = floor (tdelta / (fromIntegral (animFrameSwitch anim) / 1000.0)) `mod` noFrames
  in
    animFrames anim !! animIndex

picturizePlayer :: Platform p => Player -> GameDataM p Picture
picturizePlayer p = do
  gd <- get
  let
    ticks = gdCurrentTicks gd
    pp = playerPosition p
    playerImage :: Text
    playerImage
      | playerMode p == Air = "player_fly_" ++ playerDirection
      | playerStands || isNothing (playerWalkSince p) = "player_stand"
      | otherwise = currentAnimFrame (fromJust (playerWalkSince p)) ticks playerWalkAnim
    playerStands = abs (((^. _x) . playerVelocity) p) <= 0.01
    playerWalkAnim = lookupAnimSafe gd ("player_walk_" ++ playerDirection)
    playerDirection = if ((^. _x) . playerVelocity) p <= 0.0 then "left" else "right"
  return (pp `pictureTranslated` pictureSpriteCentered playerImage)

{-
drawCross :: Surface -> Point2 Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 5 0)) (p + (Point2 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 0 5)) (p + (Point2 0 5))

toDoubleLine :: LineSegment (Point2 Int) -> LineSegment (PointReal)
toDoubleLine = fmap (fmap fromIntegral)
-}
