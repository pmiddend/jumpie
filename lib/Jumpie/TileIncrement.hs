{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jumpie.TileIncrement where

import ClassyPrelude hiding(Real)
import Jumpie.Types
import Jumpie.GameConfig

newtype TileIncrement = TileIncrement { tileIncrementRelative :: Int } deriving(Num)

tileIncrementAbsReal :: TileIncrement -> Real
tileIncrementAbsReal = fromIntegral . (*gcTileSize) . tileIncrementRelative
