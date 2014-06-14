module Jumpie.GameData(
  GameData(..),
  updateKeydowns,
  updateTicks,
  GameDataM,
  runGame
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, get, gets, put, runStateT)


import           Graphics.UI.SDL.Types      (Event, Renderer)
import           Jumpie.GameConfig          (gcTimeMultiplier)

import           Control.Monad.Random       (RandT, evalRandT)
import           Jumpie.ImageData           (AnimMap, SurfaceMap)
import           Jumpie.SDLHelper           (processKeydowns)
import           Jumpie.Time                (GameTicks, TimeDelta (TimeDelta),
                                             getTicks, tickDelta)
import           Jumpie.Types               (Keydowns)
import           Prelude                    (div, (*), (-))
import           System.IO                  (IO)
import           System.Random              (StdGen)

data GameData = GameData {
                gdSurfaces     :: SurfaceMap,
                gdAnims        :: AnimMap,
                gdRenderer     :: Renderer,
                gdCurrentTicks :: !GameTicks,
                gdTimeDelta    :: !TimeDelta,
                gdKeydowns     :: !Keydowns
              }

type GameDataBaseM = StateT GameData IO

type GameDataM = RandT StdGen GameDataBaseM

updateTicks :: GameDataM ()
updateTicks = do
  oldTicks <- gets gdCurrentTicks
  newTicks <- liftIO getTicks
  s <- get
  put s {
    gdCurrentTicks = newTicks,
    gdTimeDelta = (TimeDelta gcTimeMultiplier) * (newTicks `tickDelta` oldTicks)
    }

updateKeydowns :: [Event] -> GameDataM ()
updateKeydowns events = do
  oldKeydowns <- gets gdKeydowns
  s <- get
  put s { gdKeydowns = processKeydowns oldKeydowns events }

runGame :: StdGen -> GameData -> GameDataM a -> IO (a,GameData)
runGame r gameData game = runStateT (evalRandT game r) gameData
