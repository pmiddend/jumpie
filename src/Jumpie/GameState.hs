module Jumpie.GameState(
  GameState(..)
  ) where

import           Data.Bool         (Bool)
import           Jumpie.GameObject (GameObject)
import Data.Int(Int)

data GameState = GameState {
    gsObjects :: [GameObject]
  , gsGameOver :: Bool
  , gsStarsCollected :: Int
  , gsStarsTotal :: Int
  }
