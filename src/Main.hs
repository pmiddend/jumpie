module Main where

import Control.Applicative((<$>),(*>),(<|>))
import Jumpie.Maybe(ifMaybe)
import Data.Ord((<),(<=),(>=),(>))
import Control.Monad(return,unless,mapM,filterM,join)
import Data.Monoid(First(First),getFirst,mconcat)
import Data.Bool(Bool(..),(&&))
import Data.Word(Word64)
import Data.Eq(Eq)
import Data.Foldable(traverse_)
import Data.Function(($),(.))
import Debug.Trace(trace)
import Text.Show(Show,show)
import Data.Functor(fmap)
import Data.Int(Int)
import Data.List(any,lookup,(++),zip,map,concatMap,filter,null,head,length,find)
import Data.Maybe(Maybe(..),isJust,isNothing,fromJust)
import Data.String(String)
import Data.Tuple(fst,snd)
import Data.Word(Word8)
import Graphics.UI.SDL.Events(Event(Quit,KeyDown))
import Graphics.UI.SDL.Image(load)
import Graphics.UI.SDL.Keysym(Keysym(..),SDLKey(SDLK_ESCAPE))
import Graphics.UI.SDL.Types(SurfaceFlag(SWSurface),Surface,surfaceGetWidth,surfaceGetHeight)
import Graphics.UI.SDL.Video(setVideoMode)
import Graphics.UI.SDL.WindowManagement(setCaption)
import Graphics.UI.SDL(withInit,InitFlag(InitEverything),flip)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Jumpie.Geometry.Intersection(rectLineSegmentIntersects)
import Jumpie.Geometry.Point(Point2(..),vmult)
import Jumpie.Geometry.Rect(Rect(Rect),topLeft,bottomRight,dimensions,top,center,right,left)
import Jumpie.SDLHelper(blitAtPosition,fillSurface,surfaceBresenham,pollEvents)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+),(*),Integral)
import System.Clock(Clock(Monotonic),sec,nsec,getTime,TimeSpec(TimeSpec))
import System.Directory(getDirectoryContents,doesFileExist)
import System.FilePath
import System.IO(IO)

-- Hilfsfunktion
traceShowId :: Show a => a -> a
traceShowId v = trace (show v) v

-- Enginetypen Anfang
newtype GameTicks = GameTicks { tickValue :: Word64 }

newtype SurfaceId = SurfaceId { surfaceId :: String } deriving(Eq)

type RectInt = Rect (Point2 Int)

type SurfaceData = (Surface,RectInt)

type SurfaceMap = [(SurfaceId,SurfaceData)]

data GameData = GameData {
                gdSurfaces :: SurfaceMap,
                gdScreen :: Surface
              }

type Real = Double
type PointReal = Point2 Real
type RectReal = Rect PointReal
type LineSegmentReal = LineSegment PointReal
-- Enginetypen Ende

-- Spieltypen Anfang
-- Objekte
newtype SensorLine = SensorLine { line :: LineSegment PointReal }
newtype Box = Box { box :: RectReal }

newtype TimeDelta = TimeDelta { timeDelta :: Double }

-- Spieler
data PlayerMode = Ground | Air deriving(Eq)

data Player = Player {
  playerPosition :: PointReal,
  playerMode :: PlayerMode,
  playerVelocity :: PointReal
  }

data GameObject = ObjectPlayer Player | ObjectBox Box | ObjectSensorLine SensorLine

isBox :: GameObject -> Bool
isBox (ObjectBox _) = True
isBox _ = False

isPlayer :: GameObject -> Bool
isPlayer (ObjectPlayer _) = True
isPlayer _ = False

isSensorLine :: GameObject -> Bool
isSensorLine (ObjectSensorLine _) = True
isSensorLine _ = False

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

gcWSSize :: Real
gcWSSize = 10.0

gcPlayerHeight :: Real
gcPlayerHeight = 20.0

gcGrv :: Real
gcGrv = 0.21875

gcTimeMultiplier = 30.0
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
  playerMode = Air,
  playerVelocity = Point2 0.0 0.0
  }

initialBoxes :: [Box]
initialBoxes = map toBox [0..boxesPerScreen-1]
  where rectSize = 35
        y = fromIntegral $ screenHeight `div` 2 - rectSize `div` 2
        boxesPerScreen = screenWidth `div` rectSize
        toBox xRaw = Box $ Rect {
          topLeft = Point2 (fromIntegral (xRaw*rectSize)) y,
          bottomRight = Point2 (fromIntegral ((xRaw+1)*rectSize)) (y + fromIntegral rectSize)
          }

initialGameState :: [GameObject]
initialGameState = ObjectPlayer initialPlayer : (ObjectBox <$> initialBoxes)

processGame :: TimeDelta -> GameState -> [IncomingAction] -> GameState
processGame t gameObjects actions = concatMap (processGameObject t gameObjects actions) gameObjects

processGameObject :: TimeDelta -> [GameObject] -> [IncomingAction] -> GameObject -> [GameObject]
processGameObject t os ias o = case o of
  ObjectPlayer p -> processPlayerObject t os ias p
  ObjectBox b -> [ObjectBox b]
  ObjectSensorLine _ -> []

processPlayerObject :: TimeDelta -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processPlayerObject t os ias p = case playerMode p of
  Ground -> processGroundPlayerObject t os ias p
  Air -> processAirPlayerObject t os ias p

-- Testet, ob ein LineSegment (ein Sensor) mit der Umgebung kollidiert
lineCollision :: [GameObject] -> LineSegmentReal -> Maybe GameObject
lineCollision boxes l = (getFirst . mconcat . map (First . (collides l))) boxes
  where collides :: LineSegmentReal -> GameObject -> Maybe GameObject
        collides l1 bp@(ObjectBox b) = ifMaybe (rectLineSegmentIntersects 0.01 (box b) l1) bp

data Sensors = Sensors {
  getW :: LineSegment PointReal,
  getFL :: LineSegment PointReal,
  getFR :: LineSegment PointReal,
  getWCollision :: Maybe GameObject,
  getFLCollision :: Maybe GameObject,
  getFRCollision :: Maybe GameObject
  }

applySensors :: [GameObject] -> PointReal -> Real -> Sensors
applySensors go p wSDev = Sensors wS fSL fSR wSCollision fSLCollision fSRCollision
  where boxes = filter isBox go
        wSCollision = lineCollision boxes wS
        wS = LineSegment (Point2 wSL wSY) (Point2 wSR wSY)
        wSY = _y p + wSDev
        wSL = _x p - gcWSSize
        wSR = _x p + gcWSSize
        fSLX = _x p - 9.0
        fSRX = _x p + 9.0
        fSYTop = _y p + gcPlayerHeight
        fSYBottom = _y p + gcPlayerHeight + 16.0
        fSL = LineSegment (Point2 fSLX fSYTop) (Point2 fSLX fSYBottom)
        fSR = LineSegment (Point2 fSRX fSYTop) (Point2 fSRX fSYBottom)
        fSLCollision = lineCollision boxes fSL
        fSRCollision = lineCollision boxes fSR

processGroundPlayerObject :: TimeDelta -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processGroundPlayerObject t os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map (ObjectSensorLine . SensorLine) [getW sensors,getFL sensors,getFR sensors]
        sensors = applySensors os (playerPosition p) 4.0
        fCollision = getFLCollision sensors <|> getFRCollision sensors
        newPlayerMode = if isNothing fCollision
                        then Air
                        else Ground
        newPlayerPositionX = case getWCollision sensors of
          Just (ObjectBox (Box r)) -> if (_x . center) r < (_x . playerPosition) p
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          Nothing -> (_x . playerPosition) p + timeDelta t * (_x . playerVelocity) p
        newPlayerPositionY = case fCollision of
          Just (ObjectBox (Box r)) -> top r - gcPlayerHeight
          Nothing -> (_y . playerPosition) p
        newPlayerVelocity = if isJust (getWCollision sensors)
                            then Point2 0.0 0.0
                            else Point2 0.0 0.0
        np = Player {
          playerPosition = Point2 newPlayerPositionX newPlayerPositionY,
          playerMode = newPlayerMode,
          playerVelocity = newPlayerVelocity
          }

processAirPlayerObject :: TimeDelta -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processAirPlayerObject t os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map (ObjectSensorLine . SensorLine) [getW sensors,getFL sensors,getFR sensors]
        sensors = applySensors os (playerPosition p) 0.0
        fCollision = getFLCollision sensors <|> getFRCollision sensors
        movingDownwards = (_y . playerVelocity) p >= 0.0
        -- Ist der naechste Zustand der Bodenzustand?
        newPlayerMode = case fCollision of
          Nothing -> Air
          Just (ObjectBox (Box r)) -> if movingDownwards && (_y . playerPosition) p + gcPlayerHeight > top r
                        then Ground
                        else Air
        newPlayerPositionX = case (getWCollision sensors) of
          Just (ObjectBox (Box r)) -> if (_x . center) r < (_x . playerPosition) p
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          Nothing -> (_x . playerPosition) p + timeDelta t * (_x . playerVelocity) p
        newPlayerPositionY = (_y . playerPosition) p + timeDelta t * (_y . playerVelocity) p
        newPlayerVelocityX = if isJust (getWCollision sensors)
                             then 0.0
                             else (_x . playerVelocity) p
        newPlayerVelocityY = (_y . playerVelocity) p + timeDelta t * gcGrv
        np = Player {
          playerPosition = Point2 newPlayerPositionX newPlayerPositionY,
          playerMode = newPlayerMode,
          playerVelocity = Point2 newPlayerVelocityX newPlayerVelocityY
          }
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
  ObjectPlayer p -> blitAt gd playerImage ((playerPosition p) - ((vmult 0.5) $ toPointReal $ dimensions playerRect))
  ObjectSensorLine (SensorLine s) -> surfaceBresenham (gdScreen gd) (255,0,0) (toIntLine s)
  ObjectBox (Box b) -> blitAt gd (getImage gd (SurfaceId "platform")) (topLeft b)
  where (playerImage,playerRect) = getImageData gd (SurfaceId "player")

mainLoop :: [Event] -> GameData -> GameState -> TimeDelta -> IO GameState
mainLoop _ gameData gameState t = do
  let newGameState = processGame t gameState []
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
      let tickDiff = fromIntegral (tickValue newTicks - tickValue oldTicks) / (1000.0 * 1000.0 * 1000.0) :: Double
      newGameState <- mainLoop events gameData gameState (TimeDelta (gcTimeMultiplier * tickDiff))
      flip (gdScreen gameData)
      outerMainLoop gameData newGameState newTicks

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
