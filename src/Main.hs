module Main where

import Control.Applicative((<$>),(*>))
import Control.Monad(return,unless,mapM,filterM)
import Data.Bool(Bool(..))
import Data.Eq(Eq)
import Data.Foldable(traverse_)
import Data.Function(($),(.))
import Data.Functor(fmap)
import Data.Int(Int)
import Data.List(any,lookup,(++),zip,map,concatMap)
import Data.Maybe(Maybe(..))
import Data.String(String)
import Data.Tuple(fst,snd)
import Data.Word(Word8)
--import Debug.Trace(trace)
import Graphics.UI.SDL.Events(Event(Quit,KeyDown))
import Graphics.UI.SDL.Image(load)
import Graphics.UI.SDL.Keysym(Keysym(..),SDLKey(SDLK_ESCAPE))
import Graphics.UI.SDL.Types(SurfaceFlag(SWSurface),Surface,surfaceGetWidth,surfaceGetHeight)
import Graphics.UI.SDL.Video(setVideoMode)
import Graphics.UI.SDL.WindowManagement(setCaption)
import Graphics.UI.SDL(withInit,InitFlag(InitEverything),flip)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Jumpie.Geometry.Point(Point2(..))
import Jumpie.Geometry.Rect(Rect(Rect),topLeft,bottomRight,dimensions)
import Jumpie.SDLHelper(blitAtPosition,fillSurface,surfaceBresenham,pollEvents)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+),(*),Integral)
import System.Clock(Clock(Monotonic),nsec,getTime)
import System.Directory(getDirectoryContents,doesFileExist)
import System.FilePath
import System.IO(IO)

-- Enginetypen Anfang
newtype GameTicks = GameTicks { tickValue :: Int }

newtype SurfaceId = SurfaceId { surfaceId :: String } deriving(Eq)

type RectInt = Rect (Point2 Int)

type SurfaceData = (Surface,RectInt)

type SurfaceMap = [(SurfaceId,SurfaceData)]

data GameData = GameData {
                gdSurfaces :: SurfaceMap,
                gdScreen :: Surface
              }

type PointReal = Point2 Double
type RectReal = Rect PointReal
-- Enginetypen Ende

-- Spieltypen Anfang
data PlayerMode = Ground | Air

-- Objekte
newtype SensorLine = SensorLine { line :: LineSegment PointReal }
newtype Box = Box { box :: RectReal }

data Player = Player {
  playerPosition :: PointReal,
  playerMode :: PlayerMode
  }

data GameObject = ObjectPlayer Player | ObjectBox Box | ObjectSensorLine SensorLine

type GameState = [GameObject]

data IncomingAction = PlayerLeft | PlayerRight | PlayerUp | PlayerDown
data OutgoingAction = Collision
-- Spieltypen Ende

-- Umgebungsvariablen Anfang
screenWidth,screenHeight,screenBpp :: Int
screenWidth = 800
screenHeight = 600
screenBpp = 32
mediaDir :: String
mediaDir = "media"
-- Umgebungsvariablen Ende

getImageData :: GameData -> SurfaceId -> SurfaceData
getImageData gd sid = case lookup sid (gdSurfaces gd) of
  Nothing -> error $ "Image " ++ surfaceId sid ++ " not found!"
  Just im -> im

getImage :: GameData -> SurfaceId -> Surface
-- TODO: .:. hier?
getImage gd sid = fst $ getImageData gd sid

getImageRect :: GameData -> SurfaceId -> RectInt
getImageRect gd sid = snd $ getImageData gd sid

initialPlayer :: Player
initialPlayer = Player {
  playerPosition = Point2 (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 4.0),
  playerMode = Ground
  }

initialBoxes :: [Box]
initialBoxes = map toBox [0..boxesPerScreen+1]
  where rectSize = 35
        height = fromIntegral $ screenHeight `div` 2 - rectSize `div` 2
        boxesPerScreen = screenWidth `div` rectSize
        toBox xRaw = Box $ Rect {
          topLeft = Point2 (fromIntegral (xRaw*rectSize)) height,
          bottomRight = Point2 (fromIntegral ((xRaw+1)*rectSize)) (height + fromIntegral rectSize)
          }

initialGameState :: [GameObject]
initialGameState = ObjectPlayer initialPlayer : (ObjectBox <$> initialBoxes)

processGame :: GameState -> [IncomingAction] -> GameState
processGame gameObjects actions = concatMap (processGameObject gameObjects actions) gameObjects

processGameObject :: [GameObject] -> [IncomingAction] -> GameObject -> [GameObject]
processGameObject os ias o = case o of
  ObjectPlayer p -> processPlayerObject os ias p
  ObjectBox b -> [ObjectBox b]
  ObjectSensorLine _ -> []

processPlayerObject :: [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processPlayerObject os ias p = [ObjectPlayer p]

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
  ObjectPlayer p -> blitAt gd playerImage ((playerPosition p) - (toPointReal $ dimensions playerRect))
  ObjectSensorLine (SensorLine s) -> surfaceBresenham (gdScreen gd) (255,0,0) (toIntLine s)
  ObjectBox (Box b) -> blitAt gd (getImage gd (SurfaceId "platform")) (topLeft b)
  where (playerImage,playerRect) = getImageData gd (SurfaceId "player")

mainLoop :: Fractional a => [Event] -> GameData -> GameState -> a -> IO GameState
mainLoop _ gameData gameState _ = do
  let newGameState = processGame gameState []
  renderGame gameData newGameState
  return newGameState

blitAt :: GameData -> Surface -> PointReal -> IO ()
blitAt gd s pos = blitAtPosition s (gdScreen gd) pos

loadImage :: FilePath -> IO Surface
loadImage = load . (mediaDir </>)

outerMainLoop :: GameData -> GameState -> GameTicks -> IO ()
outerMainLoop gameData gameState oldTicks = do
  events <- pollEvents
  newTicks <- getTicks
  unless
    (outerGameOver events) $ do
      let tickDiff = fromIntegral (tickValue newTicks - tickValue oldTicks) / 1000.0 :: Double
      newGameState <- mainLoop events gameData gameState tickDiff
      flip (gdScreen gameData)
      outerMainLoop gameData newGameState newTicks

-- Vorfilterung der Events, ob das Spiel beendet werden soll
outerGameOver :: [Event] -> Bool
outerGameOver events = any outerGameOver' events
  where outerGameOver' Quit = True
        outerGameOver' (KeyDown Keysym {symKey = SDLK_ESCAPE}) = True
        outerGameOver' _ = False

getTicks :: IO GameTicks
getTicks = GameTicks . nsec <$> getTime Monotonic

loadImages :: IO SurfaceMap
loadImages = do
  unfilteredContents <- getDirectoryContents mediaDir
  filteredContents <- filterM (doesFileExist . (mediaDir </>)) unfilteredContents
  ioImages <- mapM loadImage filteredContents
  let ioImageSizes = map (\im -> Rect (Point2 0 0) (Point2 (surfaceGetWidth im) (surfaceGetHeight im))) ioImages
  return $ zip ((SurfaceId . dropExtension) <$> filteredContents) (zip ioImages ioImageSizes)

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    images <- loadImages
    setCaption "My first haskell game" []
    ticks <- getTicks
    outerMainLoop (GameData images screen) initialGameState ticks
