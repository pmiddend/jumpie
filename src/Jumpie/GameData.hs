{-# LANGUAGE NoImplicitPrelude #-}
module Jumpie.GameData(
    GameData(..)
  , updateKeydowns
  , updateTicks
  , GameDataM
  , runGame
  ) where

import qualified Data.Set as S
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
import           System.IO                  (IO)
import           System.Random              (StdGen)
import Wrench.ImageData(SurfaceMap,AnimMap)
import Wrench.Platform
import Wrench.Time
import Wrench.Event
import Wrench.KeyMovement
import           Jumpie.Types               (Keydowns)
import ClassyPrelude

data GameData p = GameData {
    gdSurfaces     :: SurfaceMap (PlatformImage p)
  , gdAnims        :: AnimMap
  , gdPlatform     :: p
  , gdCurrentTicks :: !TimeTicks
  , gdTimeDelta    :: !TimeDelta
  , gdKeydowns     :: !Keydowns
  }

type GameDataBaseM p = StateT (GameData p) IO

type GameDataM p = RandT StdGen (GameDataBaseM p)

updateTicks :: GameDataM p ()
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
  where keyUps = S.fromList ((map toKey  . filter isKeyUp) es)
        keyDowns = S.fromList ((map toKey  . filter isKeyDown) es)
        isKeyUp (Keyboard KeyUp _ _) = True
        isKeyUp _ = False
        isKeyDown (Keyboard KeyDown _ _) = True
        isKeyDown _ = False
        toKey e = case e of
          Keyboard _ _ keysym -> keysym
          _ -> undefined

updateKeydowns :: [Event] -> GameDataM p ()
updateKeydowns events = do
  oldKeydowns <- gets gdKeydowns
  s <- get
  put s { gdKeydowns = processKeydowns oldKeydowns events }

runGame :: StdGen -> GameData p -> GameDataM p a -> IO (a,GameData p)
runGame r gameData game = runStateT (evalRandT game r) gameData
