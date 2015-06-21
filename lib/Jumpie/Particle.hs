{-# LANGUAGE TemplateHaskell #-}
module Jumpie.Particle where

import Control.Lens(makeLenses,(+~),(&))
import           Jumpie.MoveableObject
import Jumpie.ParticleType
import           Jumpie.TileIncrement
import Jumpie.ParticleGravity
import           Wrench.Time 
import Jumpie.Types
import ClassyPrelude
import           Linear.V2 (_x)

data Particle =
       Particle
         { _particleType :: ParticleType
         , _particlePosition :: PointReal
         , _particleVelocity :: PointReal
         , _particleGravity :: ParticleGravity
         , _particleInception :: TimeTicks
         }
  deriving Show

$(makeLenses ''Particle)

instance MoveableObject Particle where
  moveObject p v = p & particlePosition . _x +~ (tileIncrementAbsReal v)
