{-# LANGUAGE TemplateHaskell #-}
module Jumpie.Sensors where

import ClassyPrelude hiding(Real,head,last)
import           Jumpie.Geometry.LineSegment
import           Jumpie.Types
import           Jumpie.GameObject
import Control.Lens.TH(makeLenses)

data Sensors = Sensors {
  _sensW           :: LineSegment PointReal,
  _sensFL          :: LineSegment PointReal,
  _sensFR          :: LineSegment PointReal,
  _sensCL          :: LineSegment PointReal,
  _sensCR          :: LineSegment PointReal,
  _sensWCollision  :: Maybe GameObject,
  _sensFLCollision :: Maybe GameObject,
  _sensFRCollision :: Maybe GameObject,
  _sensCLCollision :: Maybe GameObject,
  _sensCRCollision :: Maybe GameObject
  }

$(makeLenses ''Sensors)
