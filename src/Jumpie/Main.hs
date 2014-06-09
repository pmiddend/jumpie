module Main where

import           Control.Monad         (return)
import           Data.Bool             (Bool (..), (||))
import           Data.Eq               ((==))
import           Data.Function         (($), (.))
import           Data.List             (any, concatMap, filter, lookup, map,
                                        union, (\\))
import           Data.Maybe            (fromMaybe)
import           Data.String           (String)
import           Data.Tuple            (fst)
import           Foreign.C.String      (withCStringLen)
import           Graphics.UI.SDL.Enum  (Scancode, eventTypeKeyDown,
                                        eventTypeKeyUp, scancodeEscape,
                                        scancodeLeft, scancodeRight, scancodeUp,
                                        windowPosUndefined, windowPosUndefined)
import           Graphics.UI.SDL.Image (InitFlag (..), withImgInit)
import           Graphics.UI.SDL.Types (Event (QuitEvent, KeyboardEvent),
                                        Keysym (Keysym), Window)
import           Graphics.UI.SDL.Video (createRenderer, createWindow,
                                        renderPresent, renderSetLogicalSize)
import           Jumpie.FrameState     (FrameState (FrameState), fsCurrentTicks,
                                        fsKeydowns, fsTimeDelta)
import           Jumpie.Game           (processGame)
import           Jumpie.GameConfig     (gcTimeMultiplier, screenHeight,
                                        screenWidth)
import           Jumpie.GameData       (GameData (GameData), gdRenderer)
import           Jumpie.GameGeneration (generateGame)
import           Jumpie.GameState      (GameState (GameState), gsGameover)
import           Jumpie.ImageData      (readAllDescFiles)
import           Jumpie.Render         (optimizePlats, renderGame, sdlRenderAll)
import           Jumpie.SDLHelper      (pollEvents)
import           Jumpie.Time           (GameTicks, TimeDelta (TimeDelta),
                                        getTicks, tickValue)
import           Jumpie.Types          (IncomingAction (..), Keydowns)
import           Prelude               (Double, Fractional, Integral, abs, div,
                                        error, floor, fromIntegral, mod,
                                        undefined, (*), (+), (-), (/))
import           System.IO             (IO, putStrLn)
import           System.Random         (getStdGen)

kdToAction :: Scancode -> [IncomingAction]
kdToAction sc = fromMaybe [] $ lookup
                sc
                [(scancodeLeft,[PlayerLeft]),(scancodeRight,[PlayerRight]),(scancodeUp,[PlayerJump])]

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
      sdlRenderAll gameData (optimizePlats (renderGame gameData frameState newState))
      renderPresent (gdRenderer gameData)
      outerMainLoop newKeyDowns gameData newState newTicks

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns k es = (k \\ keyUps) `union` keyDowns
  where keyUps = (map toKey  . filter isKeyUp) es
        keyDowns = (map toKey  . filter isKeyDown) es
        isKeyUp (KeyboardEvent state _ _ _ _ _) = state == eventTypeKeyUp
        isKeyUp _ = False
        isKeyDown (KeyboardEvent state _ _ _ _ _) = state == eventTypeKeyDown
        isKeyDown _ = False
        toKey e = case e of
          KeyboardEvent _ _ _ _ _ (Keysym l _ _) -> l
          _ -> undefined

-- Vorfilterung der Events, ob das Spiel beendet werden soll
outerGameOver :: [Event] -> Bool
outerGameOver events = any outerGameOver' events
  where outerGameOver' (QuitEvent _ _) = True
        outerGameOver' (KeyboardEvent _ _ _ _ _ (Keysym sc _ _)) = sc == scancodeEscape
        outerGameOver' _ = False

screenAbsoluteWidth = 0
screenAbsoluteHeight = 0
windowFlags = 0

myCreateWindow :: String -> IO Window
myCreateWindow title = withCStringLen title $ \windowTitle -> do
  createWindow
    (fst windowTitle)
    windowPosUndefined
    windowPosUndefined
    screenAbsoluteWidth
    screenAbsoluteHeight
    windowFlags

main :: IO ()
main =
  withImgInit [InitPNG] $ do
    window <- myCreateWindow "jumpie 0.1"
    renderer <- createRenderer window (-1) 0
    renderSetLogicalSize renderer (fromIntegral screenWidth) (fromIntegral screenHeight)
    g <- getStdGen
    (images,anims) <- readAllDescFiles renderer
    ticks <- getTicks
    lastGameState <- outerMainLoop
                     []
                     (GameData images anims renderer)
                     (GameState (generateGame g) False)
                     ticks
    -- Gameover loop here
    putStrLn "Oh shit"
