module Jumpie.GameState(
  GameState(..)
  ) where

import           Data.Bool         (Bool)
import           Jumpie.GameObject (GameObject)

data GameState = GameState { gsObjects :: [GameObject], gsGameOver :: Bool }
