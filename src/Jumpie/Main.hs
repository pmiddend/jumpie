{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.State.Strict (get)
import           System.Random              (getStdGen)
import           Jumpie.Picturize          (picturizeGameState)
import           Control.Monad.Random       (evalRand,evalRandT)
import           Control.Monad.Writer.Strict       (runWriterT)
import Jumpie.GameConfig
import           Jumpie.Game
import           Jumpie.GameObject
import           Jumpie.GameGeneration
import qualified Jumpie.LevelGeneration as LG
import Jumpie.GameState
import Jumpie.GameData
import Jumpie.Geometry.Rect
import Jumpie.Types
import Wrench.Time
import Wrench.Rectangle
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
    if outerGameOver events || gsGameOver gameState
        then return gameState
        else do
            gameData <- get
            let incomingActions = concatMap kdToAction (gdKeydowns gameData)
            (newPlayer,newObjects,_) <-
                processGameObjects gameState incomingActions
            let newState = gameState
                    { gsPlayer = newPlayer
                    , gsGameOver = testGameOver gameState
                    , gsObjects = newObjects
                    , gsCameraPosition = updateCameraPosition (gsCameraPosition gameState) (playerPosition newPlayer)
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
main = do
  g <- getStdGen
  let
    boundingBox = Rect (V2 0 0) (V2 30 10)
    maxLen = 5
    plats = do
      startPlat <- LG.newLevelGen (0,9) maxLen []
      nextPlats <- LG.newLevelGen (0,9) maxLen startPlat
      --nextPlats' <- LG.newLevelGen boundingBox maxLen (Just (startPlat <> nextPlats))
      return (startPlat <> nextPlats)
    --plats = [LG.Platform (V2 2 2) (V2 10 2)]  
  (r,logLines) <- runWriterT (evalRandT plats g)
  mapM_ putStrLn logLines
  putStrLn (pack (LG.showPlatformsPpm boundingBox r))
{-
main :: IO ()
main = withPlatform "jumpie 0.1" (ConstantWindowSize screenWidth screenHeight) $
  \platform -> do
    (images, anims) <- readMediaFiles (loadImage platform) mediaDir
    ticks <- getTicks
    g <- getStdGen
    font <- loadFont platform (mediaDir <> "/stdfont.ttf") 15
    let
      gameData = GameData { gdSurfaces = images, gdAnims = anims, gdPlatform = platform, gdCurrentTicks = ticks, gdTimeDelta = fromSeconds 0, gdKeydowns = mempty, gdFont = font }
      (player,objects) = evalRand generateGame g
      initialGameState = GameState { gsPlayer = player,gsObjects = objects, gsGameOver = False,gsCameraPosition = V2 0 0 }
    (lastGameState, lastGameData) <- runGame g gameData (stageMainLoop initialGameState)
    _ <- runGame g lastGameData (gameoverMainLoop lastGameState)
    return ()
-}
