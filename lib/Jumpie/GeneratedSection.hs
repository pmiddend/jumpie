{-# LANGUAGE TemplateHaskell #-}
module Jumpie.GeneratedSection where

import           Control.Lens(makeLenses)
import           Jumpie.Platform
import Jumpie.GameObject
import ClassyPrelude

data GeneratedSection = GeneratedSection {
    _secPlatforms :: [Platform]
  , _secObjects :: [GameObject]
  } deriving(Show)

$(makeLenses ''GeneratedSection)
