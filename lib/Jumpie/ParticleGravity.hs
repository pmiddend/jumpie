module Jumpie.ParticleGravity where

import           ClassyPrelude hiding(Real)
import           Jumpie.Types

data ParticleGravity = ParticleAffectedByGravity Real
                     | ParticleFloating
                     deriving(Show)
