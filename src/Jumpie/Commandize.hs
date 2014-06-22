module Jumpie.Commandize(
  RenderCommand(..),
  RenderPositionMode(..),
  commandizeGameState,
  optimizePlats
  ) where

import           Control.Applicative         ((<$>))
import           Control.Monad               (return)
-- import           Control.Monad.IO.Class      (liftIO)
import           Data.Bool                   ((||))
import           Data.Eq                     ((==))
import           Data.Eq                     (Eq)
import           Data.Function               (($), (.))
import           Data.Functor                (fmap)
import           Data.Int                    (Int)
import           Data.List                   (length, (!!), (++))
import           Data.Map.Strict             ((!))
import           Data.Maybe                  (fromJust, isNothing)
import           Data.Ord                    ((<=))
import           Data.String                 (String)

import           Data.Word                   (Word8)
import           Jumpie.Monad                (concatMapM)
--import           Debug.Trace                 (trace)
import           Control.Monad.State.Strict  (get, gets)

import           Jumpie.GameConfig           (backgroundColor,
                                              gcStarWiggleHeight,
                                              gcStarWiggleSpeed)
import           Jumpie.GameData             (GameData, GameDataM, gdAnims,
                                              gdCurrentTicks, gdSurfaces)
import           Jumpie.GameObject           (Box (Box), BoxType (..),
                                              GameObject (..), Player,
                                              PlayerMode (..),
                                              SensorLine (SensorLine),
                                              Star (..), playerMode,
                                              playerPosition, playerVelocity,
                                              playerWalkSince)
import           Jumpie.GameState            (GameState, gsObjects)
import           Jumpie.Geometry.LineSegment (LineSegment)
import           Jumpie.Geometry.Point       (Point2 (..), vmult)
import           Jumpie.Geometry.Rect        (dimensions, rectTopLeft)
import           Jumpie.ImageData            (animFrameSwitch, animFrames)

import           Jumpie.Time                 (tickDelta, timeDelta)
import           Jumpie.Types                (LineSegmentInt, PointInt,
                                              PointReal)
import           Prelude                     (Double, Fractional, Integral, abs,
                                              div, error, error, floor,
                                              fromIntegral, mod, sin, undefined,
                                              (*), (+), (-), (/))
import           Text.Show                   (Show, show)

type RGBColor = (Word8,Word8,Word8)

data RenderPositionMode = RenderPositionCenter | RenderPositionTopLeft deriving(Show,Eq)

data RenderCommand = FillScreen RGBColor |
                     RenderSprite String PointInt RenderPositionMode |
                     RenderBackground String |
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
compressPlatform ((RenderSprite _ pos mode):ns) = RenderSprite ("platform" ++ (show (length ns - 1))) pos mode
compressPlatform _ = error "compressPlatform given something other than RenderSprite"

commandizeGameState :: GameState -> GameDataM [RenderCommand]
commandizeGameState gs = do
  commandizedObjects <- concatMapM commandizeObject (gsObjects gs)
  return $ FillScreen backgroundColor : RenderBackground "background" : commandizedObjects

commandizeObject :: GameObject -> GameDataM [RenderCommand]
commandizeObject ob = case ob of
  ObjectPlayer p -> commandizePlayer p
  ObjectSensorLine s -> commandizeLine s
  ObjectBox b -> commandizeBox b
  ObjectStar s -> commandizeStar s

commandizeBox :: Box -> GameDataM [RenderCommand]
commandizeBox (Box p t) = return [
  RenderSprite ("platform" ++ boxTypeToSuffix t) (floor <$> (rectTopLeft p)) RenderPositionTopLeft
  ]

commandizeStar :: Star -> GameDataM [RenderCommand]
commandizeStar (Star pos t) = do
  currentTicks <- gets gdCurrentTicks
  let wiggledPos = pos + (Point2 0 (gcStarWiggleHeight * (sin (timeDelta (currentTicks `tickDelta` t)*gcStarWiggleSpeed))))
  return [RenderSprite "star" (floor <$> wiggledPos) RenderPositionCenter]

commandizeLine :: SensorLine -> GameDataM [RenderCommand]
commandizeLine (SensorLine s) = return $ [RenderLine (255,0,0) (toIntLine s)]

boxTypeToSuffix :: BoxType -> String
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"

commandizePlayer :: Player -> GameDataM [RenderCommand]
commandizePlayer p = do
  ticks <- gdCurrentTicks <$> get
  gd <- get
  let   pp = (playerPosition p) - ((vmult 0.5) $ toPointReal $ dimensions playerRect)
        playerImage = if playerMode p == Air
                      then ("player_fly_" ++ playerDirection)
                      else if playerStands || isNothing (playerWalkSince p)
                           then "player_stand"
                           else playerImageWalk (fromJust (playerWalkSince p))
        playerStands = abs ((pX . playerVelocity) p) <= 0.01
        playerImageWalk walkSince = animFrames playerWalkAnim !! (playerImageWalkIndex walkSince)
        playerImageWalkIndex walkSince = (floor (timeDelta (ticks `tickDelta` walkSince) / (fromIntegral (animFrameSwitch playerWalkAnim) / 1000.0))) `mod` (length (animFrames playerWalkAnim))
        playerWalkAnim = (gdAnims gd) ! ("player_walk_" ++ playerDirection)
        playerDirection = if (pX . playerVelocity) p <= 0.0 then "left" else "right"
        (_,playerRect) = gdSurfaces gd ! playerImage
  return $ [RenderSprite playerImage (floor <$> pp) RenderPositionTopLeft]

toIntLine :: LineSegment (PointReal) -> LineSegment (Point2 Int)
toIntLine = fmap (fmap floor)

toPointReal :: Integral a => Point2 a -> PointReal
toPointReal p = fromIntegral <$> p

{-
drawCross :: Surface -> Point2 Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 5 0)) (p + (Point2 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 0 5)) (p + (Point2 0 5))

toDoubleLine :: LineSegment (Point2 Int) -> LineSegment (PointReal)
toDoubleLine = fmap (fmap fromIntegral)
-}
