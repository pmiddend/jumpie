module Jumpie.Render(
  renderGame) where

import Control.Applicative((*>),(<$>))
import Data.Bool((||))
import Data.Maybe(isNothing,fromJust)
import Data.Eq((==))
import Data.Foldable(traverse_)
import Data.Function(($),(.))
import Data.Functor(fmap)
import Data.Int(Int)
import Data.List((++),(!!),length)
import Data.Map.Strict((!))
import Data.Word(Word8)
import Data.Ord((<=))
import Jumpie.Geometry.LineSegment(LineSegment)
import Data.String(String)
import Jumpie.Geometry.Point(Point2(..),vmult)
import Jumpie.Geometry.Rect(rectTopLeft,dimensions)
import Jumpie.SDLHelper(blitAtPosition,fillSurface,surfaceBresenham)
import Jumpie.GameData(gdSurfaces,gdScreen,gdAnims,GameData)
import Jumpie.GameObject(playerPosition,Box(Box),GameObject(..),SensorLine(SensorLine),Player,playerMode,PlayerMode(..),playerWalkSince,playerVelocity,BoxType(..))
import Jumpie.GameState(GameState)
import Jumpie.FrameState(FrameState,fsCurrentTicks)
import Jumpie.Types(PointReal)
import Jumpie.Time(tickValue,GameTicks)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+),(*),Integral,mod,abs)
import System.IO(IO)
import Jumpie.ImageData(ImageId,animFrames,animFrameSwitch)

fillScreen :: GameData -> (Word8,Word8,Word8) -> IO ()
fillScreen gd color = fillSurface (gdScreen gd) color

renderGame :: GameData -> FrameState -> GameState -> IO ()
renderGame gd fs go = fillScreen gd backgroundColor *> traverse_ (renderObject gd fs) go

renderObject :: GameData -> FrameState -> GameObject -> IO ()
renderObject gd fs ob = case ob of
  ObjectPlayer p -> renderPlayer gd fs p
  ObjectSensorLine (SensorLine s) -> surfaceBresenham (gdScreen gd) (255,0,0) (toIntLine s)
  ObjectBox (Box p t) -> blitAt gd ("platform" ++ boxTypeToSuffix t) (rectTopLeft p)

boxTypeToSuffix :: BoxType -> String
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"

tickDiffSecs :: GameTicks -> GameTicks -> Double
tickDiffSecs a b = fromIntegral (tickValue a - tickValue b) / (1000.0 * 1000.0 * 1000.0)

renderPlayer :: GameData -> FrameState -> Player -> IO ()
renderPlayer gd fs p = blitAt gd playerImage pp
  where pp = (playerPosition p) - ((vmult 0.5) $ toPointReal $ dimensions playerRect)
        playerImage = if playerMode p == Air
                      then ("player_fly_" ++ playerDirection)
                      else if playerStands || isNothing (playerWalkSince p)
                           then "player_stand"
                           else playerImageWalk (fromJust (playerWalkSince p))
        playerStands = abs ((pX . playerVelocity) p) <= 0.01
        playerImageWalk walkSince = animFrames playerWalkAnim !! (playerImageWalkIndex walkSince)
        playerImageWalkIndex walkSince = (floor (((fsCurrentTicks fs) `tickDiffSecs` walkSince) / (fromIntegral (animFrameSwitch playerWalkAnim) / 1000.0))) `mod` (length (animFrames playerWalkAnim))
        playerWalkAnim = (gdAnims gd) ! ("player_walk_" ++ playerDirection)
        playerDirection = if (pX . playerVelocity) p <= 0.0 then "left" else "right"
        (_,playerRect) = gdSurfaces gd ! playerImage

blitAt :: GameData -> ImageId -> PointReal -> IO ()
blitAt gd image pos = blitAtPosition ((gdSurfaces gd) ! image) pos (gdScreen gd)

backgroundColor :: (Word8,Word8,Word8)
backgroundColor = (94,129,162)

                  {-
drawCross :: Surface -> Point2 Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 5 0)) (p + (Point2 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 0 5)) (p + (Point2 0 5))

toDoubleLine :: LineSegment (Point2 Int) -> LineSegment (PointReal)
toDoubleLine = fmap (fmap fromIntegral)
-}

toIntLine :: LineSegment (PointReal) -> LineSegment (Point2 Int)
toIntLine = fmap (fmap floor)

toPointReal :: Integral a => Point2 a -> PointReal
toPointReal p = fromIntegral <$> p

