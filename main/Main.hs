{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Jumpie.Picturize          (picturizeGameState)
import Jumpie.GameConfig
import           Jumpie.Game
import           Jumpie.GameGeneration
import Wrench.MouseGrabMode
import Wrench.RenderBlockMode
import Wrench.WindowSize
import Jumpie.GameState
import Jumpie.IncomingAction
import Jumpie.Platform
import Wrench.MonadGame
import qualified Wrench.Keysym as KS
import Wrench.Event
import Wrench.KeyMovement
import Wrench.Time
import ClassyPrelude
import Control.Lens((.=),use,(^?!),_Just)
import Control.Lens.Fold(maximumOf)
import Linear.V2
import Control.Monad.Random       (MonadRandom)
import Control.Monad.State.Strict(get,MonadState,execStateT)
import Control.Monad.Writer(runWriterT)
import Wrench.Picture
import Jumpie.UnitType

discretizePicture :: Picture UnitType UnitType -> Picture Int UnitType
discretizePicture = first floor

gameoverMainLoop :: (Monad m,Applicative m,MonadState GameState m,MonadGame m) => m ()
gameoverMainLoop = do
    events <- gpollEvents
    gupdateKeydowns events
    unless (outerGameOver events) $
        do 
          (grender . discretizePicture) =<< picturizeGameState =<< get
          gameoverMainLoop

stageMainLoop :: (MonadIO m,Monad m,MonadRandom m,Applicative m,MonadGame m,MonadState GameState m) => m ()
stageMainLoop = do
  events <- gpollEvents
  gupdateTicks gcTimeMultiplier
  gupdateKeydowns events
  gameOverBefore <- use gsGameOver
  unless (outerGameOver events || gameOverBefore) $ do
    kds <- gcurrentKeydowns
    let incomingActions = concatMap kdToAction kds <> concatMap keyEventToAction events
    _ <- runWriterT (processGameObjects incomingActions)
    go <- testGameOver
    gsGameOver .= go
    grender . discretizePicture =<< picturizeGameState =<< get
    stageMainLoop

keyEventToAction :: Event -> [IncomingAction]
keyEventToAction (Keyboard (KeyboardEvent{_keyMovement=KeyDown,_keySym=KS.Up})) = [PlayerJumpPressed]
keyEventToAction _ = []

kdToAction :: KS.Keysym -> [IncomingAction]
kdToAction sc = fromMaybe [] $
    lookup
        sc
        [ (KS.Left, [PlayerLeft])
        , (KS.Right, [PlayerRight])
        , (KS.Up, [PlayerJumpHeld])
        ]

-- Vorfilterung der Events, ob das Spiel beendet werden soll
outerGameOver :: [Event] -> Bool
outerGameOver = any outerGameOver'
    where outerGameOver' Quit = True
          outerGameOver' (Keyboard (KeyboardEvent{_keySym=KS.Escape})) = True
          outerGameOver' _ = False

main :: IO ()
main = runGame "media" "jumpie 0.1" (ConstantWindowSize screenWidth screenHeight) MouseGrabNo Nothing (RenderAndWait 60) $ do
    ticks <- gcurrentTicks
    (player,sections,otherObjects) <- generateGame (ticks `plusDuration` gcFirstPlatformWait)
    let
      initialGameState = GameState {
          _gsPlayer = player
        , _gsSections = sections
        , _gsOtherObjects = otherObjects
        , _gsGameOver = False
        , _gsCameraPosition = V2 0 0
        , _gsMaxDeadline = maximumOf (traverse . traverse . platDeadline) sections ^?! _Just
        }
    lastGameState <- execStateT stageMainLoop initialGameState
    _ <- execStateT gameoverMainLoop lastGameState
    return ()
