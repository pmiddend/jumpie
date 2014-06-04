module Jumpie.Render(
  renderGame,
  sdlRenderAll,
  optimizePlats,
  RenderCommand(..)) where

import Control.Applicative((<$>))
import Control.Monad(mapM_)
import Data.Bool((||))
import Data.Maybe(isNothing,fromJust)
import Data.Eq((==))
import Data.Function(($),(.))
import Data.Functor(fmap)
import Data.Int(Int)
import Data.List((++),(!!),length,concatMap)
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
import Jumpie.GameState(GameState,gsObjects)
import Jumpie.GameConfig(backgroundColor)
import Jumpie.FrameState(FrameState,fsCurrentTicks)
import Jumpie.Types(PointReal,LineSegmentInt,PointInt)
import Jumpie.Time(tickValue,GameTicks)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+),(*),Integral,mod,abs,error)
import System.IO(IO)
import Debug.Trace(trace)
import Jumpie.ImageData(ImageId,animFrames,animFrameSwitch)
import Text.Show(show,Show)
import Data.Eq(Eq)

type RGBColor = (Word8,Word8,Word8)

data RenderCommand = FillScreen RGBColor | RenderSprite String PointInt | RenderLine RGBColor LineSegmentInt deriving(Show,Eq)

-- Mutumorphismus zwischen optimizePlats und compressPlatforms
optimizePlats :: [RenderCommand] -> [RenderCommand]
optimizePlats ((p@(RenderSprite "platforml" _)):xs) = compressPlatforms [p] xs
optimizePlats (x:xs) = x:(optimizePlats xs)
optimizePlats [] = []
compressPlatforms :: [RenderCommand] -> [RenderCommand] -> [RenderCommand]
compressPlatforms ns (q@(RenderSprite "platformm" _):ys) = compressPlatforms (ns ++ [q]) ys
compressPlatforms ns (q@(RenderSprite "platformr" _):ys) = compressPlatform (ns ++ [q]) : optimizePlats ys
compressPlatforms ns (q@(RenderSprite s _):ys) = error $ "Invalid platform configuration, ends in \"" ++ s ++ "\", previous: " ++ show ns
compressPlatforms _ _ = error "Invalid platform configuration, doesn't end in \"platformr\" element"
compressPlatform :: [RenderCommand] -> RenderCommand
--compressPlatform ((RenderSprite _ pos):ns) = RenderSprite ("platform" ++ show (length ns - 1)) pos
compressPlatform ((RenderSprite _ pos):ns) = RenderSprite ("platform" ++ trace (show (length ns - 1)) (show (length ns - 1))) pos
compressPlatform _ = error "compressPlatform given something other than RenderSprite"

renderGame :: GameData -> FrameState -> GameState -> [RenderCommand]
renderGame gd fs gs = FillScreen backgroundColor : concatMap (renderObject gd fs) (gsObjects gs)

renderObject :: GameData -> FrameState -> GameObject -> [RenderCommand]
renderObject gd fs ob = case ob of
  ObjectPlayer p -> renderPlayer gd fs p
  ObjectSensorLine (SensorLine s) -> [RenderLine (255,0,0) (toIntLine s)]
  ObjectBox (Box p t) -> [RenderSprite ("platform" ++ boxTypeToSuffix t) (floor <$> (rectTopLeft p))]

sdlRenderAll :: GameData -> [RenderCommand] -> IO ()
sdlRenderAll gd = mapM_ (sdlRender gd)

sdlRender :: GameData -> RenderCommand -> IO ()
sdlRender gd ob = case ob of
  FillScreen color -> fillSurface (gdScreen gd) color
  RenderLine color lineSegment -> surfaceBresenham (gdScreen gd) color lineSegment
  RenderSprite identifier pos -> blitAt gd identifier pos

boxTypeToSuffix :: BoxType -> String
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"

tickDiffSecs :: GameTicks -> GameTicks -> Double
tickDiffSecs a b = fromIntegral (tickValue a - tickValue b) / (1000.0 * 1000.0 * 1000.0)

renderPlayer :: GameData -> FrameState -> Player -> [RenderCommand]
renderPlayer gd fs p = [RenderSprite playerImage (floor <$> pp)]
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

blitAt :: GameData -> ImageId -> PointInt -> IO ()
blitAt gd image pos = blitAtPosition ((gdSurfaces gd) ! image) pos (gdScreen gd)

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

