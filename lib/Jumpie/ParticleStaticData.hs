{-# LANGUAGE TemplateHaskell #-}
module Jumpie.ParticleStaticData where

import           Wrench.SpriteIdentifier
import Wrench.Time
import           ClassyPrelude
import           Control.Lens (makeLenses)

data ParticleStaticData = ParticleStaticData {
    _psdSprite :: SpriteIdentifier
  , _psdLifetime :: TimeDelta
  } deriving(Show)

$(makeLenses ''ParticleStaticData)
