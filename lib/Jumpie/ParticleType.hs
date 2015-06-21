module Jumpie.ParticleType where

import ClassyPrelude
import Wrench.AnimId
import           Jumpie.ParticleStaticData

data ParticleType = ParticleTypeAnimated AnimId
                  | ParticleTypeStatic ParticleStaticData
                  deriving(Show)


       
