module Jumpie.Maybe(
  headOrNothing,
  ifMaybe
  ) where

import Data.Maybe(Maybe(..),listToMaybe)
import Control.Monad(join)
import Prelude()
import Data.Function((.))
import Data.Bool(Bool(True,False))

headOrNothing :: [Maybe a] -> Maybe a
headOrNothing = join . listToMaybe

ifMaybe :: Bool -> a -> Maybe a
ifMaybe True a = Just a
ifMaybe False _ = Nothing
