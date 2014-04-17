module Main where

import Control.Applicative((<$>))
import Control.Monad(return,(>>),unless,(>>=),mapM,filterM)
import System.Directory(getDirectoryContents,doesFileExist)
import Data.Eq(Eq)
import Control.Arrow((&&&))
import Data.String(String)
import Data.Bool(Bool(..))
import Data.Function(($),(.),id)
import Data.Int(Int)
import Data.List(elem,any,lookup,(++),zip,map)
import Data.Tuple(snd)
import Debug.Trace(trace)
import Data.Maybe(Maybe(..))
import Graphics.UI.SDL.Events(Event(NoEvent,Quit,KeyDown),pollEvent)
import Graphics.UI.SDL.Image(load)
import Graphics.UI.SDL.Keysym(Keysym(..),SDLKey(SDLK_ESCAPE))
import Graphics.UI.SDL.Types(SurfaceFlag(SWSurface),Surface)
import Graphics.UI.SDL.Video(setVideoMode,blitSurface)
import Graphics.UI.SDL.WindowManagement(setCaption)
import Graphics.UI.SDL(withInit,InitFlag(InitEverything),flip)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error)
import System.Clock(Clock(Monotonic),nsec,TimeSpec(..),getTime)
import System.FilePath
import System.IO(IO)

type Coord = Double

data Point a = Point a a

getX :: Point a -> a
getX (Point x _) = x

getY :: Point a -> a
getY (Point _ y) = y

type DoublePoint = Point Double

newtype GameTicks = GameTicks { getTickValue :: Int }

newtype Direction = Direction { getDirection :: Point Bool }

data GameState = GameState {
                 playerPosition :: DoublePoint
               , playerMovement :: Direction
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

data Action = PlayerLeft | PlayerRight | PlayerUp | PlayerDown

initialGameState :: GameState
initialGameState = GameState (Point (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 2.0)) (Direction $ Point False False)

processGame :: GameState -> [Action] -> GameState
processGame gameState actions = gameState

renderGame :: GameData -> GameState -> IO ()
renderGame gameData gameState = do
  blitSurface (getImage gameData (SurfaceId "fuji")) Nothing (gdScreen gameData) Nothing
  blitSurface (getImage gameData (SurfaceId "player")) Nothing (gdScreen ) 
  return ()

mainLoop :: Fractional a => [Event] -> GameData -> GameState -> a -> IO GameState
mainLoop events gameData gameState ticks = do
  let newGameState = processGame gameState []
  renderGame gameData newGameState
  return newGameState

-- Umgebungsvariablen
screenWidth = 800
screenHeight = 600
screenBpp = 32
mediaDir = "media"

blitAtPosition :: GameData -> SurfaceId -> DoublePoint -> IO ()
blitAtPosition gd sid pos = blitSurface (getImage gd sid) Nothing destRect
  where destRect = Just $ Rect $ x y w h

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
