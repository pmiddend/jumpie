module Main where


import           Control.Monad              (replicateM, return, unless, (=<<))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Random       (evalRand)
import           Control.Monad.State.Strict (get)
import           Data.Bool                  (Bool (..), (||))
import           Data.Eq                    ((==))
import           Data.Function              (($), (.))
import           Data.List                  (any, concatMap, length, lookup,
                                             map,(++),filter)
import           Data.Maybe                 (fromMaybe)
import           Graphics.UI.SDL.Enum       (Scancode, scancodeEscape,
                                             scancodeLeft, scancodeRight,
                                             scancodeUp)
import           Graphics.UI.SDL.Image      (InitFlag (..), withImgInit)
import           Graphics.UI.SDL.Types      (Event (QuitEvent, KeyboardEvent),
                                             Keysym (..))
import           Jumpie.Commandize          (RenderCommand (RenderSprite),
                                             RenderPositionMode (..),
                                             commandizeGameState, optimizePlats)
import           Jumpie.Game                (processGameObjects, testGameOver)
import           Jumpie.GameConfig          (screenHeight, screenWidth,gcStars)
import           Jumpie.GameData            (GameData (GameData), GameDataM,
                                             gdKeydowns, runGame,
                                             updateKeydowns, updateTicks,gdCurrentTicks)
import           Jumpie.GameGeneration      (generateGame, randomStar)
import           Jumpie.GameObject          (isStar,GameObject(..))
import           Jumpie.GameState           (GameState (GameState), gsGameOver,
                                             gsObjects)
import           Jumpie.Geometry.Point      (Point2 (Point2))
import           Jumpie.ImageData           (readAllDescFiles)
import           Jumpie.Render              (render, renderAll, renderFinish)
import           Jumpie.SDLHelper           (pollEvents, withRenderer,
                                             withWindow)
import           Jumpie.Time                (TimeDelta (TimeDelta), getTicks)
import           Jumpie.Types               (IncomingAction (..))
import           Prelude                    (Double, Fractional, Integral, abs,
                                             div, error, floor, fromIntegral,
                                             mod, undefined, (*), (+), (-), (/))
import           System.IO                  (IO)
import           System.Random              (getStdGen)

gameoverMainLoop :: GameState -> GameDataM ()
gameoverMainLoop gameState = do
  events <- liftIO pollEvents
  updateKeydowns events
  unless (outerGameOver events) $ do
    renderAll =<< (return . optimizePlats) =<< commandizeGameState gameState
    render $ RenderSprite "gameover" (Point2 (screenWidth `div` 2) (screenHeight `div` 2)) RenderPositionCenter
    renderFinish
    gameoverMainLoop gameState

stageMainLoop :: GameState -> GameDataM GameState
stageMainLoop gameState = do
  events <- liftIO pollEvents
  updateTicks
  updateKeydowns events
  if (outerGameOver events || gsGameOver gameState)
    then return gameState
    else do
      gameData <- get
      let incomingActions = concatMap kdToAction (gdKeydowns gameData)
      (newObjects,actions) <- processGameObjects gameState incomingActions
      let currentStars = length . filter (== True) . map isStar $ newObjects
      remainingStars <- replicateM
                        (gcStars - currentStars)
                        (randomStar (gdCurrentTicks gameData) newObjects)
      let newState = gameState {
            gsGameOver = testGameOver gameState,
            gsObjects = newObjects ++ (map ObjectStar remainingStars)
            }
      renderAll =<< (return . optimizePlats) =<< commandizeGameState gameState
      renderFinish
      stageMainLoop newState

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
        ticks <- getTicks
        g <- getStdGen
        let gameData = GameData images anims renderer ticks (TimeDelta 0) []
        (lastGameState,lastGameData) <- runGame g gameData $ stageMainLoop (GameState (evalRand generateGame g) False)
        _ <- runGame g lastGameData (gameoverMainLoop lastGameState)
        return ()
