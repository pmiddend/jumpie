module Jumpie.FrameState(
  FrameState(FrameState),
  fsCurrentTicks,
  fsTimeDelta,
  fsKeydowns
  ) where

import Jumpie.Types(GameTicks,TimeDelta,Keydowns)

data FrameState = FrameState {
  fsCurrentTicks :: !GameTicks,
  fsTimeDelta :: !TimeDelta,
  fsKeydowns :: !Keydowns
  }
