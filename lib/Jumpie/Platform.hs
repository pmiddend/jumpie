{-# LANGUAGE TemplateHaskell #-}
module Jumpie.Platform where

import           Jumpie.Types
import Control.Lens.TH(makeLenses)
import ClassyPrelude

data Platform = Platform {
    _platLeft :: PointInt
  , _platRight :: PointInt
  } deriving(Show)

$(makeLenses ''Platform)
