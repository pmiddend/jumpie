{-# LANGUAGE TemplateHaskell #-}
module Jumpie.Platform where

import           Jumpie.Types
import           Jumpie.GameConfig
import           Jumpie.Geometry.Rect
import Control.Lens(Getter,to,(^.))
import Control.Lens.TH(makeLenses)
import ClassyPrelude hiding(Real)
import Wrench.Time
import Linear.V2

data Platform = Platform {
    _platLeft :: Int
  , _platLength :: Int
  , _platHeight :: Int
  , _platDeadline :: TimeTicks
  } deriving(Show)

$(makeLenses ''Platform)

platRight :: Getter Platform Int
platRight = to (\p -> p ^. platLeft + p ^. platLength)

platLeftAbsReal :: Getter Platform Real
platLeftAbsReal = to (\p -> p ^. platLeftTopAbsReal . _x)

platRightAbsReal :: Getter Platform Real
platRightAbsReal = to (\p -> p ^. platRectAbsReal . right)

platLeftTopAbsReal :: Getter Platform PointReal
platLeftTopAbsReal = to (\p -> (fromIntegral . (*gcTileSize)) <$> V2 (p ^. platLeft) (p ^. platHeight))

platRectAbsReal :: Getter Platform RectReal
platRectAbsReal = to helper
  where
    helper p =
      let
        rectLeftTop = p ^. platLeftTopAbsReal
        rectRightBottom = rectLeftTop + ((fromIntegral . (*gcTileSize)) <$> V2 (p ^. platLength) 1)
      in
        Rect rectLeftTop rectRightBottom
