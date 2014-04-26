module Jumpie.Maybe(
  headOrNothing) where

import Data.Maybe(Maybe,listToMaybe)
import Control.Monad(join)
import Prelude()
import Data.Function((.))

headOrNothing :: [Maybe a] -> Maybe a
headOrNothing = join . listToMaybe
