module Jumpie.GameState(
  GameState(GameState),
  gsObjects,
  gsGameover,
  gsCurrentTicks,
  gsTimeDelta,
  gsKeydowns,
  gsGameData,
  GameStateM,
  runGame,
  sgsGameData,
  sgsCurrentTicks
  ) where

import Jumpie.GameObject(GameObject)
import Data.Bool(Bool)
import Jumpie.Types(Keydowns)
import Jumpie.Time(GameTicks,TimeDelta)
import Control.Monad((>>=),return)
import Control.Monad.State.Strict(StateT,runStateT,get)
import System.IO(IO)
import Jumpie.GameData(GameData)
import Data.Function((.))

data GameState = GameState {
  gsObjects :: [GameObject],
  gsGameover :: !Bool,
  gsCurrentTicks :: !GameTicks,
  gsTimeDelta :: !TimeDelta,
  gsKeydowns :: !Keydowns,
  gsGameData :: GameData
  }

sgsInspect :: (GameState -> a) -> GameStateM a
sgsInspect f = get >>= (return . f)

sgsGameData :: GameStateM GameData
sgsGameData = sgsInspect gsGameData

sgsCurrentTicks :: GameStateM GameTicks
sgsCurrentTicks = sgsInspect gsCurrentTicks

type GameStateM = StateT GameState IO

runGame :: GameStateM a -> GameState -> IO (a,GameState)
runGame = runStateT
