module Jumpie.GameData(
  GameData(GameData),
  gdSurfaces,
  gdAnims,
  gdRenderer,
  gdRandomGen,
  gdCurrentTicks,
  gdTimeDelta,
  gdKeydowns,
  updateKeydowns,
  updateTicks,
  GameDataM,
  runGame
  ) where

import           Control.Applicative        ((<$>))

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)




import           Graphics.UI.SDL.Types      (Event, Renderer)
import           Jumpie.GameConfig          (gcTimeMultiplier)

import           Jumpie.ImageData           (AnimMap, SurfaceMap)
import           Jumpie.SDLHelper           (processKeydowns)
import           Jumpie.Time                (GameTicks, TimeDelta (TimeDelta),
                                             getTicks, tickDelta)
import           Jumpie.Types               (Keydowns, PointInt)
import           Prelude                    (div, (*), (-))
import           System.IO                  (IO)
import           System.Random              (StdGen)

data GameData = GameData {
                gdSurfaces     :: SurfaceMap,
                gdAnims        :: AnimMap,
                gdRenderer     :: Renderer,
                gdRandomGen    :: StdGen,
                gdCurrentTicks :: !GameTicks,
                gdTimeDelta    :: !TimeDelta,
                gdKeydowns     :: !Keydowns
              }

type GameDataM = StateT GameData IO

updateTicks :: GameDataM ()
updateTicks = do
  oldTicks <- gdCurrentTicks <$> get
  newTicks <- liftIO getTicks
  s <- get
  put s {
    gdCurrentTicks = newTicks,
    gdTimeDelta = (TimeDelta gcTimeMultiplier) * (newTicks `tickDelta` oldTicks)
    }

updateKeydowns :: [Event] -> GameDataM ()
updateKeydowns events = do
  oldKeydowns <- gdKeydowns <$> get
  s <- get
  put s { gdKeydowns = processKeydowns oldKeydowns events }

runGame :: GameDataM a -> GameData -> IO (a,GameData)
runGame = runStateT
