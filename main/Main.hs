{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Jumpie.Picturize          (picturizeGameState)
import Jumpie.GameConfig
import           Jumpie.Game
import           Jumpie.GameGeneration
import Jumpie.GameState
import Jumpie.GameObject
import Jumpie.MonadGame
import Jumpie.Types
import qualified Wrench.Keysym as KS
import Wrench.Event
import Wrench.Time
import Wrench.Platform hiding(renderFinish,pollEvents,renderBegin)
import ClassyPrelude
import Control.Lens((.=),use,(^?!),_Just)
import Control.Lens.Fold(maximumOf)
import Linear.V2
import Control.Monad.Random       (MonadRandom)
import Control.Monad.State.Strict(get,MonadState,execStateT)
import Control.Monad.Writer(runWriterT)

gameoverMainLoop :: (Monad m,Applicative m,MonadState GameState m,MonadGame m) => m ()
gameoverMainLoop = do
    events <- gpollEvents
    gupdateKeydowns events
    unless (outerGameOver events) $
        do 
          grender =<< picturizeGameState =<< get
          gameoverMainLoop

stageMainLoop :: (MonadIO m,Monad m,MonadRandom m,Applicative m,MonadGame m,MonadState GameState m) => m ()
stageMainLoop = do
  events <- gpollEvents
  gupdateTicks
  gupdateKeydowns events
  gameOverBefore <- use gsGameOver
  unless (outerGameOver events || gameOverBefore) $ do
    kds <- gcurrentKeydowns
    let incomingActions = concatMap kdToAction kds
    _ <- runWriterT (processGameObjects incomingActions)
    go <- testGameOver
    gsGameOver .= go
    grender =<< picturizeGameState =<< get
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
    (player,sections) <- generateGame (ticks `plusDuration` gcFirstPlatformWait)
    let
      initialGameState = GameState {
          _gsPlayer = player
        , _gsSections = sections
        , _gsTempSection = []
        , _gsGameOver = False
        , _gsCameraPosition = V2 0 0
        , _gsMaxDeadline = maximumOf (traverse . traverse . _ObjectBox . boxDeadline) sections ^?! _Just
        }
    lastGameState <- execStateT stageMainLoop initialGameState
    _ <- execStateT gameoverMainLoop lastGameState
    return ()
