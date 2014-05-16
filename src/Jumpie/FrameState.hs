module Jumpie.FrameState(
  FrameState(FrameState),
  fsCurrentTicks,
  fsTimeDelta,
  fsKeydowns
  ) where

import Jumpie.Types(Keydowns)
import Jumpie.Time(GameTicks,TimeDelta)

data FrameState = FrameState {
  fsCurrentTicks :: !GameTicks,
  fsTimeDelta :: !TimeDelta,
  fsKeydowns :: !Keydowns
  }
