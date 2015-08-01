{-# LANGUAGE OverloadedStrings #-}
module Jumpie.Picturize(
  RenderPositionMode(..),
  picturizeGameState
  ) where

import           ClassyPrelude
import           Control.Lens              (ix, (^.), (^?!))
import           Data.Maybe                (fromJust)
import           Jumpie.GameConfig         (screenHeight, screenWidth)
import           Jumpie.GameObject
import           Jumpie.GameState
import           Jumpie.Particle
import           Jumpie.ParticleStaticData
import           Jumpie.ParticleType
import           Jumpie.Platform
import           Jumpie.Player
import           Jumpie.PlayerMode
import           Jumpie.UnitType
import           Linear.V2
import           Wrench.Animation
import           Wrench.MonadGame
import           Wrench.Picture
import           Wrench.RenderPositionMode
import           Wrench.SpriteIdentifier
import           Wrench.Time

{-
platToBoxes :: [PointInt] -> Platform -> [Box]
platToBoxes plats (Platform (V2 l y) (V2 r _) deadline) = map toBox [l..r]
  where toBox x = Box (bpos x) deadline (btype x)
        bpos x = (fmap . fmap) (fromIntegral . (*gcTileSize)) (Rect (V2 x y) (V2 (x+1) (y+1)))
        btype x
          | hasLeft && hasRight = BoxMiddle
          | hasLeft = BoxLeft
          | hasRight = BoxRight
          | otherwise = BoxSingleton
          where hasLeft = V2 (x - 1) y `elem` plats
                hasRight = V2 (x + 1) y `elem` plats
-}


picturizeGameState :: (Monad m,Applicative m,MonadGame m) => GameState -> m (Picture UnitType UnitType)
picturizeGameState gs = do
  picturizedObjects <- traverse picturizeObject (gs ^. gsAllObjects)
  let
    backgroundPicture = pictureSpriteResampled "background" RenderPositionTopLeft (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))
  return (backgroundPicture <> (-(gs ^. gsCameraPosition)) `pictureTranslated` pictures picturizedObjects)

picturizeObject :: (Monad m,Applicative m,MonadGame m) => GameObject -> m (Picture UnitType UnitType)
picturizeObject ob = case ob of
  ObjectPlayer p -> picturizePlayer p
--  ObjectSensorLine s -> picturizeLine s
  ObjectPlatform b -> picturizePlatform b
  ObjectParticle s -> picturizeParticle s

picturizePlatform :: (MonadGame m,Monad m,Functor m) => Platform -> m (Picture UnitType UnitType)
picturizePlatform p = do
  ticks <- gcurrentTicks
  let
    platLen = pack (show (p ^. platLength))
    platTopLeft = (p ^. platLeftTopAbsReal)
  if ticks < p ^. platDeadline
    then return (platTopLeft `pictureTranslated` pictureSpriteTopLeft ("platform" <> platLen))
    else do
      anim <- glookupAnimUnsafe ("platform" <> platLen <> "_crack")
      let image = currentAnimFrame (p ^. platDeadline) ticks anim
      return (platTopLeft `pictureTranslated` pictureSpriteTopLeft image)

picturizeParticle :: (Functor m,Monad m,MonadGame m) => Particle -> m (Picture UnitType UnitType)
picturizeParticle Particle{_particleType=t,_particlePosition=pos,_particleInception=inception} = do
  ticks <- gcurrentTicks
  case t of
    ParticleTypeAnimated animId -> do
      anim <- glookupAnimUnsafe animId
      let image = currentAnimFrame inception ticks anim
      return (pos `pictureTranslated` pictureSpriteCentered image)
    ParticleTypeStatic (ParticleStaticData{_psdSprite=identifier}) -> do
      return (pos `pictureTranslated` pictureSpriteCentered identifier)

--picturizeLine :: (Applicative m,MonadGame m) => SensorLine -> m (Picture UnitType UnitType)
--picturizeLine (SensorLine s) = pure (pictureLine (s ^. lineSegmentFrom) (s ^. lineSegmentTo))

currentAnimFrame :: TimeTicks -> TimeTicks -> Animation -> SpriteIdentifier
currentAnimFrame animStart currentTicks' anim =
  let
    tdelta = toSeconds (currentTicks' `tickDelta` animStart)
    noFrames = length (anim ^. animFrames)
    animIndex = floor (tdelta / (fromIntegral (anim ^. animFrameSwitch) / 1000.0)) `mod` noFrames
  in
    anim ^. animFrames ^?! ix animIndex

picturizePlayer :: (Functor m,Monad m,MonadGame m) => Player -> m (Picture UnitType UnitType)
picturizePlayer p = do
  ticks <- gcurrentTicks
  let
    pp = p ^. playerPosition
    playerStands = abs (p ^. playerVelocity ^. _x) <= 0.01
    playerDirection = if p ^. playerVelocity ^. _x <= 0.0 then "left" else "right"
  playerWalkAnim <- glookupAnimUnsafe ("player_walk_" <> playerDirection)
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
