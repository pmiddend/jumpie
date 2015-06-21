module Jumpie.MoveableObject where

import Jumpie.TileIncrement

class MoveableObject p where
  moveObject :: p -> TileIncrement -> p
