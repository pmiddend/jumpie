module Jumpie.Render(
  renderGame,
  render,
  renderAll,
  optimizePlats,
  RenderPositionMode(..),
  renderBox,
  RenderCommand(..)) where

import           Control.Applicative         ((<$>))
import           Control.Monad               (mapM_, return, (>>))
import           Data.Bool                   ((||))
import           Data.Eq                     ((==))
import           Data.Eq                     (Eq)
import           Data.Function               (($), (.))
import           Data.Functor                (fmap)
import           Data.Int                    (Int)
import           Data.List                   (concatMap, length, (!!), (++))
import           Data.Map.Strict             ((!))
import           Data.Maybe                  (fromJust, isNothing)
import           Data.Ord                    ((<=))
import           Data.String                 (String)
import           Data.Tuple                  (snd)
import           Data.Word                   (Word8)
--import           Debug.Trace                 (trace)
import           Graphics.UI.SDL.Video       (renderClear, setRenderDrawColor)
import           Jumpie.GameConfig           (backgroundColor)
import           Jumpie.GameData             (GameData, gdAnims, gdRenderer,
                                              gdSurfaces)
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
import           Jumpie.ImageData            (ImageId, animFrameSwitch,
                                              animFrames)
import           Jumpie.SDLHelper            (blitAtPosition)
import           Jumpie.Time                 (GameTicks, tickValue)
import           Jumpie.Types                (LineSegmentInt, PointInt,
                                              PointReal)
import           Prelude                     (Double, Fractional, Integral, abs,
                                              div, error, error, floor,
                                              fromIntegral, mod, undefined, (*),
                                              (+), (-), (/))
import           System.IO                   (IO)
import           Text.Show                   (Show, show)

type RGBColor = (Word8,Word8,Word8)

data RenderPositionMode = RenderPositionCenter | RenderPositionTopLeft deriving(Show,Eq)

data RenderCommand = FillScreen RGBColor |
                     RenderSprite String PointInt RenderPositionMode |
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

renderGame :: GameStateM [RenderCommand]
renderGame = do
  gs <- sgsGameState
  renderedObjects <- concatMapM renderObject (gsObjects gs)
  return $ FillScreen backgroundColor : renderedObjects

renderBox :: Box -> GameStateM [RenderCommand]
renderBox (Box p t) = return [RenderSprite ("platform" ++ boxTypeToSuffix t) (floor <$> (rectTopLeft p)) RenderPositionTopLeft]

renderStar :: Star -> GameStateM [RenderCommand]
renderStar (Star pos _) = return [RenderSprite "star" (floor <$> pos) RenderPositionCenter]

renderObject :: GameObject -> GameStateM [RenderCommand]
renderObject ob = case ob of
  ObjectPlayer p -> renderPlayer p
  ObjectSensorLine (SensorLine s) -> return $ [RenderLine (255,0,0) (toIntLine s)]
  ObjectBox b -> renderBox b
  ObjectStar s -> renderStar s

renderAll :: [RenderCommand] -> GameStateM ()
renderAll = mapM_ render

render :: RenderCommand -> GameStateM ()
render ob = case ob of
  FillScreen (r,g,b) -> do
    gd <- sgsGameData
    liftIO $ setRenderDrawColor (gdRenderer gd) r g b 255
    liftIO $ renderClear (gdRenderer gd)
  RenderLine _ _ -> return ()
  --RenderLine color lineSegment -> surfaceBresenham (gdScreen gd) color lineSegment
  RenderSprite identifier pos mode -> blitAt identifier pos mode

blitAt :: ImageId -> PointInt -> RenderPositionMode -> GameStateM ()
blitAt image pos mode = do
  gd <- sgsGameData
  let imageData = (gdSurfaces gd) ! image
      realPos = case mode of
        RenderPositionCenter -> pos - ((`div` 2) <$> (dimensions $ snd $ imageData))
        RenderPositionTopLeft -> pos
  blitAtPosition imageData realPos (gdRenderer gd)

renderPlayer :: Player -> [RenderCommand]
renderPlayer p = do
  gd <- sgsGameData
  ticks <- sgsCurrentTicks
  let playerImage = if playerMode p == Air
                      then ("player_fly_" ++ playerDirection)
                      else if playerStands || isNothing (playerWalkSince p)
                           then "player_stand"
                           else playerImageWalk (fromJust (playerWalkSince p))
      playerStands = abs ((pX . playerVelocity) p) <= 0.01
      playerImageWalk walkSince = animFrames playerWalkAnim !! (playerImageWalkIndex walkSince)
      playerImageWalkIndex walkSince = (floor ((ticks `tickDiffSecs` walkSince) / (fromIntegral (animFrameSwitch playerWalkAnim) / 1000.0))) `mod` (length (animFrames playerWalkAnim))
      playerWalkAnim = (gdAnims gd) ! ("player_walk_" ++ playerDirection)
      playerDirection = if (pX . playerVelocity) p <= 0.0 then "left" else "right"
      (_,playerRect) = gdSurfaces gd ! playerImage
  return [RenderSprite playerImage (floor <$> pp) RenderPositionTopLeft]

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

tickDiffSecs :: GameTicks -> GameTicks -> Double
tickDiffSecs a b = fromIntegral (tickValue a - tickValue b) / (1000.0 * 1000.0 * 1000.0)

boxTypeToSuffix :: BoxType -> String
boxTypeToSuffix BoxMiddle = "m"
boxTypeToSuffix BoxLeft = "l"
boxTypeToSuffix BoxRight = "r"
boxTypeToSuffix BoxSingleton = "s"
