module Jumpie.GameState(
  GameState(GameState),
  gsObjects,
  gsGameover
  ) where

import Jumpie.GameObject(GameObject)
import Data.Bool(Bool)

data GameState = GameState { gsObjects :: [GameObject], gsGameover :: Bool }
