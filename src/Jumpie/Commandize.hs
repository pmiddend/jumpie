{-# LANGUAGE OverloadedStrings #-}
module Jumpie.Commandize(
  RenderCommand(..),
  RenderPositionMode(..),
  commandizeGameState,
  optimizePlats
  ) where

import Data.List((!!))
import           Data.Map.Strict             ((!))
import           Data.Maybe                  (fromJust)
--import           Debug.Trace                 (trace)
import           Control.Monad.State.Strict  (get, gets)

import           Jumpie.GameConfig           (backgroundColor,
                                              gcStarWiggleHeight,
                                              gcStarWiggleSpeed,screenWidth,screenHeight)
import           Jumpie.GameData             (GameData, GameDataM, gdAnims,
                                              gdCurrentTicks, gdSurfaces)
import           Jumpie.GameObject           (Box (Box), BoxType (..),
                                              GameObject (..), Player,
                                              PlayerMode (..),
                                              SensorLine (SensorLine),line,
                                              Star (..), playerMode,
                                              playerPosition, playerVelocity,
                                              playerWalkSince)
import           Jumpie.GameState            (GameState, gsObjects)
import           Jumpie.Geometry.LineSegment (LineSegment,lineSegmentFrom,lineSegmentTo)
import           Jumpie.Geometry.Rect        (rectTopLeft)
import           Jumpie.Types                (LineSegmentInt, PointInt,
                                              PointReal)
import Control.Lens((^.))
import Wrench.Color
import Wrench.ImageData
import Wrench.Time
import Wrench.Rectangle
import Wrench.Picture
import ClassyPrelude
import Linear.Vector((^*))
import Linear.V2
import Wrench.RenderPositionMode

type RGBColor = (Word8,Word8,Word8)

data RenderCommand = FillScreen Color |
                     RenderSprite Text PointInt RenderPositionMode |
                     RenderBackground Text |
                     RenderLine RGBColor LineSegmentInt deriving(Show,Eq)

-- Mutumorphismus zwischen optimizePlats und compressPlatforms
optimizePlats :: [RenderCommand] -> [RenderCommand]
optimizePlats ((p@(RenderSprite "platformr" _ _)):xs) = compressPlatforms [p] xs
optimizePlats (x:xs) = x:(optimizePlats xs)
optimizePlats [] = []
compressPlatforms :: [RenderCommand] -> [RenderCommand] -> [RenderCommand]
compressPlatforms ns (q@(RenderSprite "platformm" _ _):ys) = compressPlatforms (ns ++ [q]) ys
compressPlatforms ns (q@(RenderSprite "platforml" _ _):ys) = compressPlatform (ns ++ [q]) : optimizePlats ys
compressPlatforms ns (a:as) =
  error $ "Invalid platform configuration, ends in \"" ++ show a ++ "\", next: " ++ show as ++ ", previous: " ++ show ns
compressPlatforms _ [] =
  error "Invalid platform configuration, doesn't end in \"platforml\" element, ends in nothing"
compressPlatform :: [RenderCommand] -> RenderCommand
compressPlatform ((RenderSprite _ pos mode):ns) = RenderSprite ("platform" ++ (pack (show (length ns - 1)))) pos mode
compressPlatform _ = error "compressPlatform given something other than RenderSprite"

commandizeGameState :: GameState -> GameDataM p Picture
commandizeGameState gs = do
  commandizedObjects <- mapM commandizeObject (gsObjects gs)
  return (pictures ([pictureSpriteResampled "background" RenderPositionTopLeft (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))] <> commandizedObjects))

commandizeObject :: GameObject -> GameDataM p Picture
commandizeObject ob = case ob of
  ObjectPlayer p -> commandizePlayer p
  ObjectSensorLine s -> commandizeLine s
  ObjectBox b -> commandizeBox b
  ObjectStar s -> commandizeStar s

commandizeBox :: Box -> GameDataM p Picture
commandizeBox (Box p t) = return ((rectTopLeft p) `pictureTranslated` (pictureSpriteTopLeft ("platform" ++ boxTypeToSuffix t)))

commandizeStar :: Star -> GameDataM p Picture
commandizeStar (Star pos t) = do
  currentTicks <- gets gdCurrentTicks
  let wiggledPos = pos + (V2 0 (gcStarWiggleHeight * (sin (toSeconds (currentTicks `tickDelta` t)*gcStarWiggleSpeed))))
  return (wiggledPos `pictureTranslated` (pictureSpriteCentered "star"))

commandizeLine :: SensorLine -> GameDataM p Picture
commandizeLine (SensorLine s) = return (pictureLine (lineSegmentFrom s) (lineSegmentTo s))

boxTypeToSuffix :: IsString s => BoxType -> s
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"

commandizePlayer :: Player -> GameDataM p Picture
commandizePlayer p = do
  ticks <- gdCurrentTicks <$> get
  gd <- get
  let   pp = (playerPosition p) - ((^* 0.5) $ (playerRect ^. rectangleDimensions))
        playerImage :: Text
        playerImage = if playerMode p == Air
                      then ("player_fly_" ++ playerDirection)
                      else if playerStands || isNothing (playerWalkSince p)
                           then "player_stand"
                           else playerImageWalk (fromJust (playerWalkSince p))
        playerStands = abs (((^. _x) . playerVelocity) p) <= 0.01
        playerImageWalk walkSince = animFrames playerWalkAnim !! (playerImageWalkIndex walkSince)
        playerImageWalkIndex walkSince = (floor (toSeconds (ticks `tickDelta` walkSince) / (fromIntegral (animFrameSwitch playerWalkAnim) / 1000.0))) `mod` (length (animFrames playerWalkAnim))
        playerWalkAnim = (gdAnims gd) ! ("player_walk_" ++ playerDirection)
        playerDirection = if ((^. _x) . playerVelocity) p <= 0.0 then "left" else "right"
        (_,playerRect) = gdSurfaces gd ! playerImage
  return (pp `pictureTranslated` (pictureSpriteTopLeft playerImage))

{-
drawCross :: Surface -> Point2 Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 5 0)) (p + (Point2 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 0 5)) (p + (Point2 0 5))

toDoubleLine :: LineSegment (Point2 Int) -> LineSegment (PointReal)
toDoubleLine = fmap (fmap fromIntegral)
-}
