module Jumpie.GameState(
  GameState(..)
  ) where

import           Jumpie.GameObject (GameObject)
import ClassyPrelude

data GameState = GameState {
    gsObjects :: [GameObject]
  , gsGameOver :: Bool
  , gsStarsCollected :: Int
  , gsStarsTotal :: Int
  }
