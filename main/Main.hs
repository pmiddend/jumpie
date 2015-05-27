{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State.Strict (get)
import           System.Random              (getStdGen)
import           Jumpie.Picturize          (picturizeGameState)
import           Control.Monad.Random       (evalRand)
import Jumpie.GameConfig
import           Jumpie.Game
import           Jumpie.GameObject
import           Jumpie.GameGeneration
import Jumpie.GameState
import Jumpie.GameData
import Jumpie.Types
import Wrench.Time
import qualified Wrench.Keysym as KS
import Wrench.Engine (withPlatform)
import Wrench.Event
import Wrench.ImageData (readMediaFiles)
import Wrench.Platform hiding(renderFinish,pollEvents,renderBegin)
import ClassyPrelude
import Control.Lens((^.))
import Linear.V2

gameoverMainLoop :: Platform p => GameState -> GameDataM p ()
gameoverMainLoop gameState = do
    events <- pollEvents
    updateKeydowns events
    unless (outerGameOver events) $
        do 
          render =<< picturizeGameState gameState
          gameoverMainLoop gameState

stageMainLoop :: Platform p => GameState -> GameDataM p GameState
stageMainLoop gameState = do
    events <- pollEvents
    updateTicks
    updateKeydowns events
    if outerGameOver events || (gameState ^. gsGameOver)
        then return gameState
        else do
            gameData <- get
            let incomingActions = concatMap kdToAction (gdKeydowns gameData)
            (newPlayer,newCameraPosition,newTempSection,newSections,_) <-
                processGameObjects gameState incomingActions
            let newState = gameState
                    { _gsPlayer = newPlayer
                    , _gsGameOver = testGameOver gameState
                    , _gsSections = newSections
                    , _gsTempSection = newTempSection
                    , _gsCameraPosition = updateCameraPosition newCameraPosition (newPlayer ^. playerPosition)
                    }
            render =<< picturizeGameState gameState
            stageMainLoop newState

kdToAction :: KS.Keysym -> [IncomingAction]
kdToAction sc = fromMaybe [] $
    lookup
        sc
        [ (KS.Left, [PlayerLeft])
        , (KS.Right, [PlayerRight])
        , (KS.Up, [PlayerJump])]

-- Vorfilterung der Events, ob das Spiel beendet werden soll
outerGameOver :: [Event] -> Bool
outerGameOver = any outerGameOver'
    where outerGameOver' Quit = True
          outerGameOver' (Keyboard _ _ KS.Escape) = True
          outerGameOver' _ = False

updateCameraPosition :: PointReal -> PointReal -> PointReal
updateCameraPosition cameraPos playerPos =
  let
    k = cameraPos ^. _x
    px = playerPos ^. _x
    w = fromIntegral screenWidth
    z = fromIntegral cameraTolerance
    x = min (max k ((w-z)/2 + px - w)) (px - (w-z)/2)
  in
    V2 x (cameraPos ^. _y)

main :: IO ()
main = withPlatform "jumpie 0.1" (ConstantWindowSize screenWidth screenHeight) $
  \platform -> do
    (images, anims) <- readMediaFiles (loadImage platform) mediaDir
    ticks <- getTicks
    g <- getStdGen
    font <- loadFont platform (mediaDir <> "/stdfont.ttf") 15
    let
      gameData = GameData { gdSurfaces = images, gdAnims = anims, gdPlatform = platform, gdCurrentTicks = ticks, gdTimeDelta = fromSeconds 0, gdKeydowns = mempty, gdFont = font }
      (player,sections) = evalRand (generateGame ticks) g
      initialGameState = GameState { _gsPlayer = player,_gsSections = sections, _gsTempSection = [], _gsGameOver = False,_gsCameraPosition = V2 0 0 }
    (lastGameState, lastGameData) <- runGame g gameData (stageMainLoop initialGameState)
    _ <- runGame g lastGameData (gameoverMainLoop lastGameState)
    return ()
