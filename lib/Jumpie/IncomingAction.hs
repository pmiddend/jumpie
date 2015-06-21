module Jumpie.IncomingAction where

import ClassyPrelude

data IncomingAction = PlayerLeft
                    | PlayerRight
                    | PlayerJumpPressed
                    | PlayerJumpHeld
  deriving Eq
