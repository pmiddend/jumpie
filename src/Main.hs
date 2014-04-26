module Main where

import Control.Applicative((<$>),(*>),(<|>))
import Data.Ord((<),(<=),(>=))
import Control.Monad(return,unless,mapM,filterM,join)
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
import Jumpie.Geometry.Intersection(rectLineSegmentIntersection)
import Jumpie.Geometry.Point(Point2(..),vmult)
import Jumpie.Geometry.Rect(Rect(Rect),topLeft,bottomRight,dimensions)
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
-- Enginetypen Ende

-- Spieltypen Anfang
-- Objekte
newtype SensorLine = SensorLine { line :: LineSegment PointReal }
newtype Box = Box { box :: RectReal }

newtype TimeDelta = TimeDelta { timeDelta :: Double }

-- Spieler
data PlayerMode = Ground | Air

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
        height = fromIntegral $ screenHeight `div` 2 - rectSize `div` 2
        boxesPerScreen = screenWidth `div` rectSize
        toBox xRaw = Box $ Rect {
          topLeft = Point2 (fromIntegral (xRaw*rectSize)) height,
          bottomRight = Point2 (fromIntegral ((xRaw+1)*rectSize)) (height + fromIntegral rectSize)
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

lineCollision :: [GameObject] -> LineSegment (Point2 Double) -> Maybe (Point2 Real)
lineCollision boxes l = join $ find isJust collisions
  where collisions :: [Maybe (Point2 Real)]
        collisions = map (\(ObjectBox b) -> rectLineSegmentIntersection 0.01 (box b) l) boxes

processGroundPlayerObject :: TimeDelta -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processGroundPlayerObject t os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map ObjectSensorLine [SensorLine wS,SensorLine fSL,SensorLine fSR]
        wS = LineSegment (Point2 wSL wSY) (Point2 wSR wSY)
        wSY = (_y . playerPosition) p + 4.0
        wSL = (_x . playerPosition) p - gcWSSize
        wSR = (_x . playerPosition) p + gcWSSize
        fsLX = (_x . playerPosition) p - 9.0
        fsRX = (_x . playerPosition) p + 9.0
        fsYTop = (_y . playerPosition) p + gcPlayerHeight
        fsYBottom = (_y . playerPosition) p + gcPlayerHeight + 16.0
        fSL = LineSegment (Point2 fsLX fsYTop) (Point2 fsLX fsYBottom)
        fSR = LineSegment (Point2 fsRX fsYTop) (Point2 fsRX fsYBottom)
        boxes = filter isBox os
        fsCollision = (lineCollision boxes fSL) <|> (lineCollision boxes fSR)
        notGrounded = isNothing fsCollision
        newPlayerMode = if notGrounded
                        then Air
                        else Ground
        wSCollision = lineCollision boxes wS
        newPlayerPositionX = case wSCollision of
          Just (Point2 wsCx wsCy) -> if wsCx < (_x . playerPosition) p
                                     then wsCx + 11
                                     else wsCx - 11
          Nothing -> (_x . playerPosition) p + timeDelta t * (_x . playerVelocity) p
        newPlayerPositionY = case fsCollision of
          Just (Point2 fsCx fsCy) -> fsCy - 20.0
          Nothing -> (_y . playerPosition) p + timeDelta t * (_y . playerVelocity) p
        newPlayerVelocity = if isJust wSCollision
                            then Point2 0.0 0.0
                            else playerVelocity p
        np = Player {
          playerPosition = Point2 newPlayerPositionX newPlayerPositionY,
          playerMode = newPlayerMode,
          playerVelocity = newPlayerVelocity
          }

processAirPlayerObject :: TimeDelta -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processAirPlayerObject t os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map ObjectSensorLine [SensorLine wS,SensorLine fSL,SensorLine fSR]
        wS = LineSegment (Point2 wSL wSY) (Point2 wSR wSY)
        wSY = (_y . playerPosition) p
        wSL = (_x . playerPosition) p - gcWSSize
        wSR = (_x . playerPosition) p + gcWSSize
        fsLX = (_x . playerPosition) p - 9.0
        fsRX = (_x . playerPosition) p + 9.0
        fsYTop = (_y . playerPosition) p + gcPlayerHeight
        fsYBottom = (_y . playerPosition) p + gcPlayerHeight + 16.0
        fSL = LineSegment (Point2 fsLX fsYTop) (Point2 fsLX fsYBottom)
        fSR = LineSegment (Point2 fsRX fsYTop) (Point2 fsRX fsYBottom)
        boxes = filter isBox os
        fsCollision = (lineCollision boxes fSL) <|> (lineCollision boxes fSR)
        movingDownwards = (_y . playerVelocity) p >= 0.0
        --grounded = traceShowId $ movingDownwards && isJust fsCollision && (_y (fromJust fsCollision)) < ((_y . playerPosition) p + gcPlayerHeight)
        grounded = traceShowId $ movingDownwards && isJust fsCollision
        newPlayerMode = if grounded
                        then Ground
                        else Air
        wSCollision = lineCollision boxes wS
        newPlayerPositionX = case wSCollision of
          Just (Point2 wsCx wsCy) -> if wsCx < (_x . playerPosition) p
                                     then wsCx + gcWSSize + 1.0
                                     else wsCx - (gcWSSize + 1.0)
          Nothing -> (_x . playerPosition) p + timeDelta t * (_x . playerVelocity) p
        newPlayerPositionY = (_y . playerPosition) p + timeDelta t * (_y . playerVelocity) p
        newPlayerVelocityX = if isJust wSCollision
                             then 0.0
                             else (_x . playerVelocity) p
        newPlayerVelocityY = if grounded
                             then 0.0
                             else (_y . playerVelocity) p + timeDelta t * gcGrv
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
