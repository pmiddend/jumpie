module Jumpie.GameData(
  GameData(GameData),
  gdSurfaces,
  gdAnims,
  gdScreen) where

import Jumpie.ImageData(SurfaceMap,AnimMap)
import Graphics.UI.SDL.Types(Surface)

data GameData = GameData {
                gdSurfaces :: SurfaceMap,
                gdAnims :: AnimMap,
                gdScreen :: Surface
              }
