module Jumpie.Debug(
  traceShowId
  ) where

import Text.Show(Show)
import Debug.Trace(trace)

traceShowId :: Show a => a -> a
traceShowId v = trace (show v) v
