module Jumpie.GameState(
    GameState(..)
  , gsAllObjects
  ) where

import           Jumpie.GameObject (GameObject(..),Player)
import ClassyPrelude

data GameState = GameState {
    gsObjects :: [GameObject]
  , gsPlayer :: Player
  , gsGameOver :: Bool
  }

gsAllObjects :: GameState -> [GameObject]
gsAllObjects gs = gsPlayerPacked gs : gsObjects gs

gsPlayerPacked :: GameState -> GameObject
gsPlayerPacked = ObjectPlayer . gsPlayer
