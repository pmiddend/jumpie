{-# LANGUAGE TemplateHaskell #-}
module Jumpie.GameState where

import           Jumpie.GameObject
import           Jumpie.Player
import ClassyPrelude
import Jumpie.Types
import Jumpie.Platforms
import Control.Lens.TH
import Control.Lens((^.))
import Control.Lens.Getter(to,Getter)
import Wrench.Time

data GameState = GameState {
    _gsSections :: [Platforms]
  , _gsOtherObjects :: [GameObject]
  , _gsPlayer :: Player
  , _gsGameOver :: Bool
  , _gsCameraPosition :: PointReal
  , _gsMaxDeadline :: TimeTicks
  }

$(makeClassy ''GameState)

gsAllObjects :: Getter GameState [GameObject]
gsAllObjects = to (\gs -> gs ^. gsPlayerPacked : (gs ^. gsOtherObjects <> (ObjectPlatform <$> (concat (gs ^. gsSections)))))

gsPlayerPacked :: Getter GameState GameObject
gsPlayerPacked = to (ObjectPlayer . (^. gsPlayer))
