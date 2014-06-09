module Main where

import           Control.Monad         (return)
import           Data.Bool             (Bool (..), (||))
import           Data.Eq               ((==))
import           Data.Function         (($))
import           Data.List             (any, concatMap, lookup)
import           Data.Map              ((!))
import           Data.Maybe            (fromMaybe)
import           Data.Tuple            (snd)
import           Graphics.UI.SDL.Enum  (Scancode, scancodeEscape, scancodeLeft,
                                        scancodeRight, scancodeUp)
import           Graphics.UI.SDL.Image (InitFlag (..), withImgInit)
import           Graphics.UI.SDL.Types (Event (QuitEvent, KeyboardEvent),
                                        Keysym (..))
import           Jumpie.FrameState     (FrameState (FrameState), fsCurrentTicks,
                                        fsKeydowns, fsTimeDelta)
import           Jumpie.Game           (processGame)
import           Jumpie.GameConfig     (gcTimeMultiplier, screenHeight,
                                        screenWidth)
import           Jumpie.GameData       (GameData (GameData), gdRenderer,
                                        gdSurfaces)
import           Jumpie.GameGeneration (generateGame)
import           Jumpie.GameState      (GameState (GameState), gsGameover)
import           Jumpie.Geometry.Point (Point2 (Point2), pX, pY)
import           Jumpie.Geometry.Rect  (dimensions)
import           Jumpie.ImageData      (readAllDescFiles)
import           Jumpie.Render         (RenderCommand (RenderSprite),
                                        optimizePlats, render, renderAll,
                                        renderGame)
import           Jumpie.SDLHelper      (pollEvents, processKeydowns,
                                        renderFinish, withRenderer, withWindow)
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

gameoverMainLoop gameData gameState oldTicks = do
  events <- pollEvents
  if outerGameOver events
    then return ()
    else do
      let gameoverRect = dimensions $ snd $ (gdSurfaces gameData) ! "gameover"
      renderAll gameData (optimizePlats (renderGame gameData oldTicks gameState))
      render gameData $ RenderSprite "gameover" $ Point2 (screenWidth `div` 2 - pX gameoverRect `div` 2) (screenHeight `div` 2 - pY gameoverRect `div` 2)
      renderFinish (gdRenderer gameData)
      gameoverMainLoop gameData gameState oldTicks

stageMainLoop :: Keydowns -> GameData -> GameState -> GameTicks -> IO GameState
stageMainLoop oldKeydowns gameData gameState oldTicks = do
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
      renderAll gameData (optimizePlats (renderGame gameData (fsCurrentTicks frameState) newState))
      renderFinish (gdRenderer gameData)
      stageMainLoop newKeyDowns gameData newState newTicks

-- Vorfilterung der Events, ob das Spiel beendet werden soll
outerGameOver :: [Event] -> Bool
outerGameOver events = any outerGameOver' events
  where outerGameOver' (QuitEvent _ _) = True
        outerGameOver' (KeyboardEvent _ _ _ _ _ (Keysym sc _ _)) = sc == scancodeEscape
        outerGameOver' _ = False

main :: IO ()
main =
  withImgInit [InitPNG] $ do
    withWindow "jumpie 0.1" $ \window -> do
      withRenderer window screenWidth screenHeight $ \renderer -> do
        (images,anims) <- readAllDescFiles renderer
        let gameData = GameData images anims renderer
        g <- getStdGen
        ticks <- getTicks
        lastGameState <- stageMainLoop
                         []
                         gameData
                         (GameState (generateGame g) False)
                         ticks
        newTicks <- getTicks
        gameoverMainLoop gameData lastGameState newTicks
        putStrLn "Oh shit"
