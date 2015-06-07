{-# LANGUAGE TemplateHaskell #-}
module Jumpie.Platform where

import           Jumpie.Types
import Control.Lens.TH(makeLenses)
import ClassyPrelude
import Wrench.Time

data Platform = Platform {
    _platLeft :: PointInt
  , _platRight :: PointInt
  , _platDeadline :: TimeTicks
  } deriving(Show)

$(makeLenses ''Platform)
