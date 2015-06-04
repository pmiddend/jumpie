{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Jumpie.Picturize          (picturizeGameState)
import Jumpie.GameConfig
import           Jumpie.Game
import           Jumpie.GameObject
import           Jumpie.GameGeneration
import Jumpie.GameState
import Jumpie.MonadGame
import Jumpie.Types
import qualified Wrench.Keysym as KS
import Wrench.Event
import Wrench.Platform hiding(renderFinish,pollEvents,renderBegin)
import ClassyPrelude
import Control.Lens((^.))
import Linear.V2
import Control.Monad.Random       (MonadRandom)

gameoverMainLoop :: (Monad m,Applicative m,MonadGame m) => GameState -> m ()
gameoverMainLoop gameState = do
    events <- gpollEvents
    gupdateKeydowns events
    unless (outerGameOver events) $
        do 
          grender =<< picturizeGameState gameState
          gameoverMainLoop gameState

stageMainLoop :: (MonadIO m,Monad m,MonadRandom m,Applicative m,MonadGame m) => GameState -> m GameState
stageMainLoop gameState = do
    events <- gpollEvents
    gupdateTicks
    gupdateKeydowns events
    if outerGameOver events || (gameState ^. gsGameOver)
        then return gameState
        else do
            kds <- gcurrentKeydowns
            let incomingActions = concatMap kdToAction kds
            (newPlayer,newCameraPosition,newTempSection,newSections,_) <-
                processGameObjects gameState incomingActions
            let newState = gameState
                    { _gsPlayer = newPlayer
                    , _gsGameOver = testGameOver gameState
                    , _gsSections = newSections
                    , _gsTempSection = newTempSection
                    , _gsCameraPosition = updateCameraPosition newCameraPosition (newPlayer ^. playerPosition)
                    }
            grender =<< picturizeGameState gameState
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
    lastGameState <- stageMainLoop initialGameState
    gameoverMainLoop lastGameState
    return ()
