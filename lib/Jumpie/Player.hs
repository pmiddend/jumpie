{-# LANGUAGE TemplateHaskell #-}
module Jumpie.Player where

import ClassyPrelude
import Jumpie.Types
import Jumpie.PlayerMode
import Wrench.Time
import Control.Lens(makeLenses,(+~),(&))
import Jumpie.MoveableObject
import Jumpie.TileIncrement
import Linear.V2(_x)

data Player = Player {
    _playerPosition  :: PointReal
  , _playerMode      :: PlayerMode
  , _playerVelocity  :: PointReal
  , _playerWalkSince :: Maybe TimeTicks
  } deriving(Show)

$(makeLenses ''Player)

instance MoveableObject Player where
  moveObject p v = p & playerPosition . _x +~ (tileIncrementAbsReal v)
