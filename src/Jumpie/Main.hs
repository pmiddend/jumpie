{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State.Strict (get)
import           System.Random              (getStdGen)
import           Jumpie.Commandize          (commandizeGameState)
import           Jumpie.List                (countBy)
import           Control.Monad.Random       (evalRand)
import Jumpie.GameConfig
import           Jumpie.Game
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

gameoverMainLoop :: Platform p => GameState -> GameDataM p ()
gameoverMainLoop gameState = do
    events <- pollEvents
    updateKeydowns events
    unless (outerGameOver events) $
        do 
          render =<< (commandizeGameState gameState)
          gameoverMainLoop gameState

stageMainLoop :: Platform p => GameState -> GameDataM p GameState
stageMainLoop gameState = do
    events <- pollEvents
    updateTicks
    updateKeydowns events
    if (outerGameOver events || gsGameOver gameState || gsStarsCollected
                                                            gameState ==
                                                        gsStarsTotal gameState)
        then return gameState
        else do
            gameData <- get
            let incomingActions = concatMap kdToAction (gdKeydowns gameData)
            (newObjects,actions) <-
                processGameObjects gameState incomingActions
            let newState = gameState
                    { gsGameOver = testGameOver gameState
                    , gsObjects = newObjects{- ++
                      (map ObjectStar remainingStars)-}
                    , gsStarsCollected = gsStarsCollected gameState +
                      (countBy isStarCollected actions)
                    }
            render =<< (commandizeGameState gameState)
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
outerGameOver events = any outerGameOver' events
    where outerGameOver' Quit = True
          outerGameOver' (Keyboard _ _ KS.Escape) = True
          outerGameOver' _ = False

main :: IO ()
main = withPlatform "jumpie 0.1" (ConstantWindowSize screenWidth screenHeight) $
  \platform -> do
    (images, anims) <- readMediaFiles (loadImage platform) mediaDir
    ticks <- getTicks
    g <- getStdGen
    font <- loadFont platform (mediaDir <> "/stdfont.ttf") 15
    let gameData = GameData { gdSurfaces = images, gdAnims = anims, gdPlatform = platform, gdCurrentTicks = ticks, gdTimeDelta = fromSeconds 0, gdKeydowns = mempty, gdFont = font }
    let initialGameState = GameState { gsObjects = (evalRand generateGame g), gsGameOver = False, gsStarsCollected = 0, gsStarsTotal = 10 }
    (lastGameState, lastGameData) <- runGame g gameData $
                                       stageMainLoop initialGameState
    _ <- runGame g lastGameData (gameoverMainLoop lastGameState)
    return ()
