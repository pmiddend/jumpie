module Main where

import Control.Monad(return)
import Data.Bool(Bool(..),(||))
import Data.Function(($),(.))
import Data.List(any,map,(\\),union,filter,concatMap)
import Graphics.UI.SDL(withInit,InitFlag(InitEverything),flip)
import Graphics.UI.SDL.Events(Event(Quit,KeyDown,KeyUp))
import System.Random(getStdGen)
import Graphics.UI.SDL.Image(load)
import Graphics.UI.SDL.Keysym(Keysym(..),SDLKey(SDLK_ESCAPE))
import Graphics.UI.SDL.Keysym(SDLKey(..))
import Graphics.UI.SDL.Types(SurfaceFlag(SWSurface),Surface)
import Graphics.UI.SDL.Video(setVideoMode)
import Graphics.UI.SDL.WindowManagement(setCaption)
import Jumpie.Game(processGame)
import Jumpie.GameConfig(gcTimeMultiplier,screenWidth,screenHeight,screenBpp,mediaDir)
import Jumpie.ImageData(readAllDescFiles)
import Jumpie.SDLHelper(pollEvents)
import Jumpie.GameData(GameData(GameData),gdScreen)
import Jumpie.GameState(GameState(GameState),gsGameover)
import Jumpie.FrameState(FrameState(FrameState),fsTimeDelta,fsKeydowns,fsCurrentTicks)
import Jumpie.Types(IncomingAction(..),Keydowns)
import Jumpie.Time(TimeDelta(TimeDelta),tickValue,GameTicks,getTicks)
import Jumpie.GameGeneration(generateGame)
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+),(*),Integral,mod,abs)
import System.FilePath
import System.IO(IO,putStrLn)
import Jumpie.Render(renderGame,sdlRenderAll)

kdToAction :: SDLKey -> [IncomingAction]
kdToAction SDLK_LEFT = [PlayerLeft]
kdToAction SDLK_RIGHT = [PlayerRight]
kdToAction SDLK_UP = [PlayerJump]
kdToAction _ = []

loadImage :: FilePath -> IO Surface
loadImage = load . (mediaDir </>)

outerMainLoop :: Keydowns -> GameData -> GameState -> GameTicks -> IO GameState
outerMainLoop oldKeydowns gameData gameState oldTicks = do
  events <- pollEvents
  newTicks <- getTicks
  if (outerGameOver events || gsGameover gameState)
    then return gameState
    else do
      let tickDiff = fromIntegral (tickValue newTicks - tickValue oldTicks) / (1000.0 * 1000.0 * 1000.0) :: Double
      let newKeyDowns = processKeydowns oldKeydowns events
      let frameState = FrameState {
        fsTimeDelta = TimeDelta (gcTimeMultiplier * tickDiff),
        fsCurrentTicks = newTicks,
        fsKeydowns = newKeyDowns
        }
      let incomingActions = concatMap kdToAction newKeyDowns
      let newState = processGame frameState gameState incomingActions
      sdlRenderAll gameData (renderGame gameData frameState newState)
      flip (gdScreen gameData)
      outerMainLoop newKeyDowns gameData newState newTicks

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

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    g <- getStdGen
    {-
    -- Randomplatforms
    let boundingRect = (Rect (Point2 0 0) (Point2 21 21))
    let randomPlats = randomPlatforms g (Rect (Point2 0 0) (Point2 20 20)) 5
    -- Valid platforms
    --putStrLn $ showPlatformsPpm boundingRect $ randomPlats
    let validPlats = take 20 $ validPlatforms randomPlats
    putStrLn $ "Number of plats: " ++ (show (length validPlats))
    putStrLn $ "Plats: " ++ (show validPlats)
    putStrLn $ showPlatformsPpm boundingRect $ validPlats
    -}
    (images,anims) <- readAllDescFiles
    setCaption "jumpie 0.1" []
    ticks <- getTicks
    lastGameState <- outerMainLoop [] (GameData images anims screen) (GameState (generateGame g) False) ticks
    -- Gameover loop here
    putStrLn "Oh shit"
