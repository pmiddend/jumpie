{-# LANGUAGE TemplateHaskell #-}
module Jumpie.GameState where

import           Jumpie.GameObject
import           Jumpie.GameGeneration
import ClassyPrelude
import Jumpie.Types
import Control.Lens.TH
import Control.Lens((^.))
import Control.Lens.Getter(to,Getter)
import Wrench.Time

data GameState = GameState {
    _gsSections :: [WorldSection]
  , _gsTempSection :: WorldSection
  , _gsPlayer :: Player
  , _gsGameOver :: Bool
  , _gsCameraPosition :: PointReal
  , _gsMaxDeadline :: TimeTicks
  }

$(makeClassy ''GameState)

gsAllObjects :: Getter GameState [GameObject]
gsAllObjects = to (\gs -> gs ^. gsPlayerPacked : (gs ^. gsTempSection ++ join (gs ^. gsSections)))

gsPlayerPacked :: Getter GameState GameObject
gsPlayerPacked = to (ObjectPlayer . (^. gsPlayer))
