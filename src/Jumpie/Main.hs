module Main where

import           Control.Monad         (return, unless)
import           Data.Bool             (Bool (..), (||))
import           Data.Eq               ((==))
import           Data.Function         (($))
import           Data.List             (any, concatMap, lookup)
import           Data.Maybe            (fromMaybe)
import           Graphics.UI.SDL.Enum  (Scancode, scancodeEscape, scancodeLeft,
                                        scancodeRight, scancodeUp)
import           Graphics.UI.SDL.Image (InitFlag (..), withImgInit)
import           Graphics.UI.SDL.Types (Event (QuitEvent, KeyboardEvent),
                                        Keysym (..))
import           Jumpie.FrameState     (FrameState (FrameState), fsCurrentTicks,
                                        fsKeydowns, fsTimeDelta)
import           Jumpie.Game           (processGameObjects)
import           Jumpie.GameConfig     (gcTimeMultiplier, screenHeight,
                                        screenWidth)
import           Jumpie.GameData       (GameData (GameData), gdRenderer)
import           Jumpie.GameGeneration (generateGame)
import           Jumpie.GameState      (GameState (GameState), gsGameover,GameStateM)
import           Jumpie.Geometry.Point (Point2 (Point2))
import           Jumpie.ImageData      (readAllDescFiles)
import           Jumpie.Render         (RenderCommand (RenderSprite),
                                        RenderPositionMode (..), optimizePlats,
                                        render, renderAll, renderGame)
import           Jumpie.SDLHelper      (pollEvents, processKeydowns,
                                        renderFinish, withRenderer, withWindow)
import           Jumpie.Time           (GameTicks, TimeDelta (TimeDelta),
                                        getTicks, tickValue)
import           Jumpie.Types          (IncomingAction (..), Keydowns)
import           Prelude               (Double, Fractional, Integral, abs, div,
                                        error, floor, fromIntegral, mod,
                                        undefined, (*), (+), (-), (/))
import           System.IO             (IO, putStrLn)
import           System.Random         (RandomGen, getStdGen)

gameoverMainLoop :: GameTicks -> GameStateM ()
gameoverMainLoop oldTicks = do
  events <- liftIO pollEvents
  unless (outerGameOver events) $ do
    gameData <- sgsGameData
    gameState <- sgsGameState
    renderAll gameData (optimizePlats (renderGame gameData oldTicks gameState))
    render gameData $ RenderSprite "gameover" (Point2 (screenWidth `div` 2) (screenHeight `div` 2)) RenderPositionCenter
    renderFinish (gdRenderer gameData)
    gameoverMainLoop gameData gameState oldTicks

stageMainLoop :: RandomGen r => Keydowns -> GameData -> GameState -> GameTicks -> r -> IO GameState
stageMainLoop oldKeydowns gameData gameState oldTicks randomGen = do
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
      let newState = processGameObjects frameState gameState incomingActions
      renderAll gameData (optimizePlats (renderGame gameData (fsCurrentTicks frameState) newState))
      renderFinish (gdRenderer gameData)
      stageMainLoop newKeyDowns gameData newState newTicks randomGen

kdToAction :: Scancode -> [IncomingAction]
kdToAction sc = fromMaybe [] $ lookup
                sc
                [(scancodeLeft,[PlayerLeft]),(scancodeRight,[PlayerRight]),(scancodeUp,[PlayerJump])]


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
                         g
        newTicks <- getTicks
        gameoverMainLoop gameData lastGameState newTicks
        putStrLn "Oh shit"
