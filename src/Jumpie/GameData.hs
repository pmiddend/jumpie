module Jumpie.GameData(
  GameData(GameData),
  gdSurfaces,
  gdAnims, gdRenderer) where

import           Graphics.UI.SDL.Types (Renderer)
import           Jumpie.ImageData      (AnimMap, SurfaceMap)

data GameData = GameData {
                gdSurfaces :: SurfaceMap,
                gdAnims    :: AnimMap,
                gdRenderer :: Renderer
              }
