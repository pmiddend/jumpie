module Jumpie.GameState(
    GameState(..)
  , gsAllObjects
  ) where

import           Jumpie.GameObject
import           Jumpie.GameGeneration
import ClassyPrelude
import Jumpie.Types

data GameState = GameState {
    gsSections :: [WorldSection]
  , gsTempSection :: WorldSection
  , gsPlayer :: Player
  , gsGameOver :: Bool
  , gsCameraPosition :: PointReal
  }

gsAllObjects :: GameState -> [GameObject]
gsAllObjects gs = gsPlayerPacked gs : (gsTempSection gs ++ join (gsSections gs))

gsPlayerPacked :: GameState -> GameObject
gsPlayerPacked = ObjectPlayer . gsPlayer
