{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Jumpie.Picturize          (picturizeGameState)
import Jumpie.GameConfig
import           Jumpie.Game
import           Jumpie.GameGeneration
import Jumpie.GameState
import Jumpie.MonadGame
import Jumpie.Types
import qualified Wrench.Keysym as KS
import Wrench.Event
import Wrench.Platform hiding(renderFinish,pollEvents,renderBegin)
import ClassyPrelude
import Control.Lens((^.),(.=),use)
import Linear.V2
import Control.Monad.Random       (MonadRandom)
import Control.Monad.State.Strict(get,MonadState,execStateT)

gameoverMainLoop :: (Monad m,Applicative m,MonadState GameState m,MonadGame m) => m ()
gameoverMainLoop = do
    events <- gpollEvents
    gupdateKeydowns events
    unless (outerGameOver events) $
        do 
          gameState <- get
          grender =<< picturizeGameState gameState
          gameoverMainLoop

stageMainLoop :: (MonadIO m,Monad m,MonadRandom m,Applicative m,MonadGame m,MonadState GameState m) => m ()
stageMainLoop = do
    events <- gpollEvents
    gupdateTicks
    gupdateKeydowns events
    gameOverBefore <- use gsGameOver
    if outerGameOver events || gameOverBefore
        then return ()
        else do
            kds <- gcurrentKeydowns
            let incomingActions = concatMap kdToAction kds
            _ <- processGameObjects incomingActions
            go <- testGameOver
            gsGameOver .= go
            gs <- get
            grender =<< picturizeGameState gs
            stageMainLoop

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

main :: IO ()
main = runGame "jumpie 0.1" (ConstantWindowSize screenWidth screenHeight) $ do
    ticks <- gcurrentTicks
    (player,sections) <- generateGame ticks
    let
      initialGameState = GameState {
          _gsPlayer = player
        , _gsSections = sections
        , _gsTempSection = []
        , _gsGameOver = False
        , _gsCameraPosition = V2 0 0
        }
    lastGameState <- execStateT stageMainLoop initialGameState
    _ <- execStateT gameoverMainLoop lastGameState
    return ()
