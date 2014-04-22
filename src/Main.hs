module Main where

import Jumpie.Geometry.Point(Point(..))
import Jumpie.Geometry.Intersection(rectIntersects)
import Data.String(String)
import Jumpie.Geometry.Rect(Rect(Rect),lineSegments)
import Data.Functor(fmap)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Jumpie.SDLHelper(blitAtPosition,fillSurface,surfaceBresenham)
import Control.Applicative((<$>))
import Control.Monad(return,unless,mapM,filterM,mapM_,when)
import System.Directory(getDirectoryContents,doesFileExist)
import Data.Eq(Eq)
import Data.Bool(Bool(..))
import Data.Function(($),(.))
import Data.Int(Int)
import Data.Word(Word8)
import Data.List(any,lookup,(++),zip,map)
--import Debug.Trace(trace)
import Data.Maybe(Maybe(..))
import Graphics.UI.SDL.Events(Event(NoEvent,Quit,KeyDown),pollEvent)
import Graphics.UI.SDL.Image(load)
import Graphics.UI.SDL.Keysym(Keysym(..),SDLKey(SDLK_ESCAPE))
import Graphics.UI.SDL.Types(SurfaceFlag(SWSurface),Surface)
import Graphics.UI.SDL.Video(setVideoMode)
import Graphics.UI.SDL.WindowManagement(setCaption)
import Graphics.UI.SDL(withInit,InitFlag(InitEverything),flip)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+))
import System.Clock(Clock(Monotonic),nsec,getTime)
import System.FilePath
import System.IO(IO,putStrLn)

newtype GameTicks = GameTicks { getTickValue :: Int }

newtype Direction = Direction { getDirection :: Point Bool }

data GameState = GameState {
                 playerPosition :: Point Double
               }

newtype SurfaceId = SurfaceId { getSurfaceId :: String } deriving(Eq)

type SurfaceMap = [(SurfaceId,Surface)]

data GameData = GameData {
                gdSurfaces :: SurfaceMap,
                gdScreen :: Surface
              }

getImage :: GameData -> SurfaceId -> Surface
getImage gd sid = case lookup sid (gdSurfaces gd) of
  Nothing -> error $ "Image " ++ getSurfaceId sid ++ " not found!"
  Just im -> im

data IncomingAction = PlayerLeft | PlayerRight | PlayerUp | PlayerDown
data OutgoingAction = Collision

initialGameState :: GameState
initialGameState = GameState (Point (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0))

processGame :: GameState -> [IncomingAction] -> GameState
processGame gameState _ = gameState

fillScreen :: GameData -> (Word8,Word8,Word8) -> IO ()
fillScreen gd color = fillSurface (gdScreen gd) color

backgroundColor :: (Word8,Word8,Word8)
backgroundColor = (94,129,162)

drawCross :: Surface -> Point Int -> IO ()
drawCross s p = do
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point 5 0)) (p + (Point 5 0))
  surfaceBresenham s (255,0,0) $ LineSegment (p - (Point 0 5)) (p + (Point 0 5))

toDoubleLine :: LineSegment (Point Int) -> LineSegment (Point Double)
toDoubleLine = fmap (fmap fromIntegral)

toIntLine :: LineSegment (Point Double) -> LineSegment (Point Int)
toIntLine = fmap (fmap floor)

renderGame :: GameData -> GameState -> IO ()
renderGame gd _ = do
  fillScreen gd backgroundColor
  blitAt gd (SurfaceId "player") (Point 30.0 30.0)
  let firstRect = Rect (Point 100.0 100.0) (Point 300.0 300.0)
  let secondRect = Rect (Point 100.0 100.0) (Point 300.0 300.0)
  mapM_ (surfaceBresenham (gdScreen gd) (255,0,0)) (map toIntLine $ lineSegments firstRect)
  mapM_ (surfaceBresenham (gdScreen gd) (255,0,255)) (map toIntLine $ lineSegments secondRect)
  when (rectIntersects 0.1 firstRect secondRect) (putStrLn "AJAJAJAJAJAJ")
  {-
  let firstLine = LineSegment (Point 100 100) (Point 200 100)
  let secondLine = LineSegment (Point 100 200) (Point 200 200)
  surfaceBresenham (gdScreen gd) (255,255,255) firstLine
  surfaceBresenham (gdScreen gd) (255,255,255) secondLine
  case lineSegmentIntersection (1/10) (toDoubleLine firstLine) (toDoubleLine secondLine) of
    Nothing -> return ()
    Just p -> drawCross (gdScreen gd) (fmap floor p)
  -}

mainLoop :: Fractional a => [Event] -> GameData -> GameState -> a -> IO GameState
mainLoop _ gameData gameState _ = do
  let newGameState = processGame gameState []
  renderGame gameData newGameState
  return newGameState

-- Umgebungsvariablen
screenWidth,screenHeight,screenBpp :: Int
screenWidth = 800
screenHeight = 600
screenBpp = 32
mediaDir :: String
mediaDir = "media"

blitAt :: GameData -> SurfaceId -> Point Double -> IO ()
blitAt gd sid pos = blitAtPosition (getImage gd sid) (gdScreen gd) pos

loadImage :: FilePath -> IO Surface
loadImage = load . (mediaDir </>)

-- Wrapper um das etwas eklige pollEvent
pollEvents :: IO [Event]
pollEvents = do
  event <- pollEvent
  case event of
    NoEvent -> return []
    _ -> do
      events <- pollEvents
      return $ event : events

outerMainLoop :: GameData -> GameState -> GameTicks -> IO ()
outerMainLoop gameData gameState oldTicks = do
  events <- pollEvents
  newTicks <- getTicks
  unless
    (outerGameOver events) $ do
      let tickDiff = fromIntegral (getTickValue newTicks - getTickValue oldTicks) / 1000.0
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

loadImages :: IO [(SurfaceId,Surface)]
loadImages = do
  unfilteredContents <- getDirectoryContents mediaDir
  filteredContents <- filterM (doesFileExist . (mediaDir </>)) unfilteredContents
  ioImages <- mapM loadImage filteredContents
  return $ zip (map (SurfaceId . dropExtension) filteredContents) ioImages

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    images <- loadImages
    setCaption "My first haskell game" []
    ticks <- getTicks
    outerMainLoop (GameData images screen) initialGameState ticks
