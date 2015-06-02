{-# LANGUAGE OverloadedStrings #-}
module Jumpie.Picturize(
  RenderPositionMode(..),
  picturizeGameState
  ) where

import           Data.Maybe                  (fromJust)
import           Jumpie.GameConfig           (screenWidth,screenHeight)
import           Jumpie.GameData
import           Jumpie.GameObject
import           Jumpie.GameState
import           Jumpie.Geometry.LineSegment (lineSegmentFrom,lineSegmentTo)
import           Jumpie.Geometry.Rect        (rectTopLeft)
import Control.Lens((^.),(^?!),ix)
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
  picturizedObjects <- traverse picturizeObject (gs ^. gsAllObjects)
  let
    backgroundPicture = pictureSpriteResampled "background" RenderPositionTopLeft (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))
  return (backgroundPicture <> (-(gs ^. gsCameraPosition)) `pictureTranslated` pictures picturizedObjects)

picturizeObject :: Platform p => GameObject -> GameDataM p Picture
picturizeObject ob = case ob of
  ObjectPlayer p -> picturizePlayer p
  ObjectSensorLine s -> picturizeLine s
  ObjectBox b -> picturizeBox b
  ObjectParticle s -> picturizeParticle s

picturizeBox :: Box -> GameDataM p Picture
picturizeBox (Box p _ t) = return ((p ^. rectTopLeft) `pictureTranslated` pictureSpriteTopLeft ("platform" ++ boxTypeToSuffix t))

picturizeParticle :: Particle -> GameDataM p Picture
picturizeParticle (Particle identifier pos inception) = do
  ticks <- currentTicks
  anim <- lookupAnimSafe identifier
  let image = currentAnimFrame inception ticks anim
  return (pos `pictureTranslated` pictureSpriteCentered image)

picturizeLine :: SensorLine -> GameDataM p Picture
picturizeLine (SensorLine s) = return (pictureLine (s ^. lineSegmentFrom) (s ^. lineSegmentTo))

boxTypeToSuffix :: IsString s => BoxType -> s
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"

currentAnimFrame :: TimeTicks -> TimeTicks -> Animation -> SpriteIdentifier
currentAnimFrame animStart currentTicks' anim =
  let
    tdelta = toSeconds (currentTicks' `tickDelta` animStart)
    noFrames = length (anim ^. animFrames)
    animIndex = floor (tdelta / (fromIntegral (anim ^. animFrameSwitch) / 1000.0)) `mod` noFrames
  in
    anim ^. animFrames ^?! ix animIndex

picturizePlayer :: Platform p => Player -> GameDataM p Picture
picturizePlayer p = do
  ticks <- currentTicks
  let
    pp = p ^. playerPosition
    playerStands = abs (p ^. playerVelocity ^. _x) <= 0.01
    playerDirection = if p ^. playerVelocity ^. _x <= 0.0 then "left" else "right"
  playerWalkAnim <- lookupAnimSafe ("player_walk_" <> playerDirection)
  let 
    playerImage :: Text
    playerImage
      | p ^. playerMode == Air = "player_fly_" <> playerDirection
      | playerStands || isNothing (p ^. playerWalkSince) = "player_stand"
      | otherwise = currentAnimFrame (fromJust (p ^. playerWalkSince)) ticks playerWalkAnim
  return (pp `pictureTranslated` pictureSpriteCentered playerImage)

{-
drawCross :: Surface -> Point2 Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 5 0)) (p + (Point2 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 0 5)) (p + (Point2 0 5))

toDoubleLine :: LineSegment (Point2 Int) -> LineSegment (PointReal)
toDoubleLine = fmap (fmap fromIntegral)
-}
