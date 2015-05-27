{-# LANGUAGE TemplateHaskell #-}
module Jumpie.GameState(
    GameState(..)
  , gsAllObjects
  , gsSections
  , gsTempSection
  , gsPlayer
  , gsGameOver
  , gsCameraPosition
  ) where

import           Jumpie.GameObject
import           Jumpie.GameGeneration
import ClassyPrelude
import Jumpie.Types
import Control.Lens.TH
import Control.Lens((^.))
import Control.Lens.Getter(to,Getter)

data GameState = GameState {
    _gsSections :: [WorldSection]
  , _gsTempSection :: WorldSection
  , _gsPlayer :: Player
  , _gsGameOver :: Bool
  , _gsCameraPosition :: PointReal
  }

$(makeClassy ''GameState)

gsAllObjects :: Getter GameState [GameObject]
gsAllObjects = to (\gs -> gs ^. gsPlayerPacked : (gs ^. gsTempSection ++ join (gs ^. gsSections)))

gsPlayerPacked :: Getter GameState GameObject
gsPlayerPacked = to (ObjectPlayer . (^. gsPlayer))
