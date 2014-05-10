module Main where

import Control.Applicative((<$>),(*>),pure)
import Control.Monad(return,unless)
import Data.Bool(Bool(..))
import Data.Eq((==))
import Data.Foldable(traverse_)
import Data.Function(($),(.))
import Data.Functor(fmap)
import Data.Int(Int)
import Jumpie.Imagedata(readAllDescFiles)
import Data.List(any,map,(\\),union,filter,concatMap)
import Data.Tuple(fst,snd)
import Data.Word(Word64)
import Data.Word(Word8)
import Graphics.UI.SDL.Events(Event(Quit,KeyDown,KeyUp))
import Graphics.UI.SDL.Image(load)
import Graphics.UI.SDL.Keysym(Keysym(..),SDLKey(SDLK_ESCAPE))
import Graphics.UI.SDL.Keysym(SDLKey(..))
import Graphics.UI.SDL.Types(SurfaceFlag(SWSurface),Surface)
import Graphics.UI.SDL.Video(setVideoMode)
import Graphics.UI.SDL.WindowManagement(setCaption)
import Graphics.UI.SDL(withInit,InitFlag(InitEverything),flip)
import Jumpie.GameConfig(gcTimeMultiplier,initialGameState,screenWidth,screenHeight,screenBpp,mediaDir)
import Jumpie.Game(processGame)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Jumpie.Geometry.Point(Point2(..),vmult)
import Jumpie.Geometry.Rect(topLeft,dimensions)
import Jumpie.SDLHelper(blitAtPosition,fillSurface,surfaceBresenham,pollEvents)
import Jumpie.Types(ImageId,IncomingAction(..),GameData(GameData),SurfaceData,gdSurfaces,RectInt,playerPosition,Box(Box),GameObject(..),gdScreen,PointReal,GameState,TimeDelta(TimeDelta),tickValue,SensorLine(SensorLine),GameTicks(GameTicks),Player,playerMode,PlayerMode(..),Keydowns,FrameState(FrameState),getTimeDelta,getKeydowns)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+),(*),Integral)
import System.Clock(Clock(Monotonic),getTime,TimeSpec(TimeSpec))
import System.FilePath
import System.IO(IO)
import Data.Map.Strict((!))

-- Umgebungsvariablen Anfang

-- Umgebungsvariablen Ende

getImageData :: GameData -> ImageId -> SurfaceData
getImageData gd sid = gdSurfaces gd ! sid

getImage :: GameData -> ImageId -> Surface
-- TODO: .:. hier?
getImage gd sid = fst $ getImageData gd sid

getImageRect :: GameData -> ImageId -> RectInt
getImageRect gd sid = snd $ getImageData gd sid

fillScreen :: GameData -> (Word8,Word8,Word8) -> IO ()
fillScreen gd color = fillSurface (gdScreen gd) color

backgroundColor :: (Word8,Word8,Word8)
backgroundColor = (94,129,162)

drawCross :: Surface -> Point2 Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 5 0)) (p + (Point2 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point2 0 5)) (p + (Point2 0 5))

toDoubleLine :: LineSegment (Point2 Int) -> LineSegment (PointReal)
toDoubleLine = fmap (fmap fromIntegral)

toIntLine :: LineSegment (PointReal) -> LineSegment (Point2 Int)
toIntLine = fmap (fmap floor)

toPointReal :: Integral a => Point2 a -> PointReal
toPointReal p = fromIntegral <$> p

renderGame :: GameData -> GameState -> IO ()
renderGame gd go = fillScreen gd backgroundColor *> traverse_ (renderObject gd) go

renderObject :: GameData -> GameObject -> IO ()
renderObject gd ob = case ob of
  ObjectPlayer p -> renderPlayer gd p
  ObjectSensorLine (SensorLine s) -> surfaceBresenham (gdScreen gd) (255,0,0) (toIntLine s)
  ObjectBox (Box b) -> blitAt gd "platform" (topLeft b)

renderPlayer :: GameData -> Player -> IO ()
renderPlayer gd p = blitAt gd playerImage pp
  where pp = (playerPosition p) - ((vmult 0.5) $ toPointReal $ dimensions playerRect)
        playerImage = if playerMode p == Air then "player_fly_right" else "player_stand"
        (_,playerRect) = gdSurfaces gd ! playerImage

mainLoop :: [Event] -> GameData -> GameState -> FrameState -> IO GameState
mainLoop _ gameData gameState frameState = renderGame gameData newState *> pure newState
  where newState = processGame frameState gameState incomingActions
        incomingActions = concatMap kdToAction (getKeydowns frameState)

kdToAction :: SDLKey -> [IncomingAction]
kdToAction SDLK_LEFT = [PlayerLeft]
kdToAction SDLK_RIGHT = [PlayerRight]
kdToAction SDLK_UP = [PlayerJump]
kdToAction _ = []

blitAt :: GameData -> ImageId -> PointReal -> IO ()
blitAt gd image pos = blitAtPosition ((gdSurfaces gd) ! image) pos (gdScreen gd)

loadImage :: FilePath -> IO Surface
loadImage = load . (mediaDir </>)

outerMainLoop :: Keydowns -> GameData -> GameState -> GameTicks -> IO ()
outerMainLoop oldKeydowns gameData gameState oldTicks = do
  events <- pollEvents
  newTicks <- getTicks
  unless
    (outerGameOver events) $ do
      let tickDiff = fromIntegral (tickValue newTicks - tickValue oldTicks) / (1000.0 * 1000.0 * 1000.0) :: Double
      let newKeyDowns = processKeydowns oldKeydowns events
      let frameState = FrameState {
        getTimeDelta = TimeDelta (gcTimeMultiplier * tickDiff),
        getKeydowns = newKeyDowns
        }
      newGameState <- mainLoop events gameData gameState frameState
      flip (gdScreen gameData)
      outerMainLoop newKeyDowns gameData newGameState newTicks

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns k es = (k \\ keyUps) `union` keyDowns
  where keyUps = (map toKey  . filter isKeyUp) es
        keyDowns = (map toKey  . filter isKeyDown) es
        isKeyUp (KeyUp _) = True
        isKeyUp _ = False
        isKeyDown (KeyDown _) = True
        isKeyDown _ = False
        toKey e = case e of
          KeyDown (Keysym l _ _) -> l
          KeyUp (Keysym l _ _) -> l
          _ -> undefined

-- Vorfilterung der Events, ob das Spiel beendet werden soll
outerGameOver :: [Event] -> Bool
outerGameOver events = any outerGameOver' events
  where outerGameOver' Quit = True
        outerGameOver' (KeyDown Keysym {symKey = SDLK_ESCAPE}) = True
        outerGameOver' _ = False

getTicks :: IO GameTicks
getTicks = do
  (TimeSpec s ns) <- getTime Monotonic
  return $ GameTicks $ ((fromIntegral s :: Word64) * 1000 * 1000 * 1000) + fromIntegral ns

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    images <- readAllDescFiles
    setCaption "jumpie 0.1" []
    ticks <- getTicks
    outerMainLoop [] (GameData images screen) initialGameState ticks
