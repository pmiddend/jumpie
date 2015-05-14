module Jumpie.GameData(
    GameData(..)
  , updateKeydowns
  , updateTicks
  , GameDataM
  , runGame
  ) where

import           Data.List                  (filter, map, union, (++), (\\))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, get, gets, put, runStateT)


import           Jumpie.GameConfig          (gcTimeMultiplier)

import           Control.Monad.Random       (RandT, evalRandT)
{-
import           Jumpie.SDLHelper           (processKeydowns)
import           Jumpie.Time                (GameTicks, TimeDelta (TimeDelta),
                                             getTicks, tickDelta)
import           Jumpie.Types               (Keydowns)
-}
import           Prelude                    (div, (*), (-))
import           System.IO                  (IO)
import           System.Random              (StdGen)
import Wrench.ImageData(SurfaceMap,AnimMap)
import Wrench.Platform
import Wrench.Time
import Wrench.Event
import           Jumpie.Types               (Keydowns)

data GameData p = GameData {
    gdSurfaces     :: SurfaceMap (PlatformImage p)
  , gdAnims        :: AnimMap
  , gdPlatform     :: Platform p
  , gdCurrentTicks :: !TimeTicks
  , gdTimeDelta    :: !TimeDelta
  , gdKeydowns     :: !Keydowns
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
    gdTimeDelta = (fromSeconds gcTimeMultiplier) * (newTicks `tickDelta` oldTicks)
    }

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns k es = (k \\ keyUps) `union` keyDowns
  where keyUps = (map toKey  . filter isKeyUp) es
        keyDowns = (map toKey  . filter isKeyDown) es
        isKeyUp (KeyboardEvent state _ _ _ _ _) = state == eventTypeKeyUp
        isKeyUp _ = False
        isKeyDown (KeyboardEvent state _ _ _ _ _) = state == eventTypeKeyDown
        isKeyDown _ = False
        toKey e = case e of
          KeyboardEvent _ _ _ _ _ (SDLT.Keysym l _ _) -> l
          _ -> undefined

updateKeydowns :: [Event] -> GameDataM ()
updateKeydowns events = do
  oldKeydowns <- gets gdKeydowns
  s <- get
  put s { gdKeydowns = processKeydowns oldKeydowns events }

runGame :: StdGen -> GameData -> GameDataM a -> IO (a,GameData)
runGame r gameData game = runStateT (evalRandT game r) gameData
