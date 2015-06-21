{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Jumpie.GameObject where

import           Jumpie.Player
import           Jumpie.SensorLine
import           Jumpie.Particle
import           Jumpie.MoveableObject
import ClassyPrelude hiding(Real)
import Jumpie.Platform
import Control.Lens.TH

data GameObject = ObjectPlayer Player
                | ObjectPlatform Platform
                | ObjectSensorLine SensorLine
                | ObjectParticle Particle
  deriving(Show)

$(makePrisms ''GameObject)

instance MoveableObject GameObject where
  moveObject (ObjectPlayer p) v = ObjectPlayer (moveObject p v)
  moveObject (ObjectPlatform p) v = ObjectPlatform (moveObject p v)
  moveObject (ObjectSensorLine p) v = ObjectSensorLine (moveObject p v)
  moveObject (ObjectParticle p) v = ObjectParticle (moveObject p v)

maybePlatform :: GameObject -> Maybe Platform
maybePlatform (ObjectPlatform b) = Just b
maybePlatform _ = Nothing

isPlatform :: GameObject -> Bool
isPlatform (ObjectPlatform _) = True
isPlatform _ = False

isPlayer :: GameObject -> Bool
isPlayer (ObjectPlayer _) = True
isPlayer _ = False

isSensorLine :: GameObject -> Bool
isSensorLine (ObjectSensorLine _) = True
isSensorLine _ = False

isParticle :: GameObject -> Bool
isParticle (ObjectParticle _) = True
isParticle _ = False
