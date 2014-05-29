module Jumpie.GameConfig(
  gcWSSize,
  gcTileSize,
  gcPlayerHeight,
  gcPlayerMaxSpeed,
  gcDec,
  gcAir,
  gcGrv,
  gcAcc,
  gcJmp,
  gcFrc,
  gcPlatCount,
  gcPlatMaxLength,
  gcTimeMultiplier,
  screenWidth,
  screenHeight,
  screenBpp,
  mediaDir
  ) where

import Jumpie.Types(Real)
import Prelude(fromIntegral,(/),(-),div,(*),(+))
import Data.Int(Int)
import Data.String(String)

screenWidth,screenHeight,screenBpp :: Int
screenWidth = 800
screenHeight = 600
screenBpp = 32
mediaDir :: String
mediaDir = "media"

gcPlatMaxLength :: Int
gcPlatMaxLength = 5

gcPlatCount :: Int
gcPlatCount = 15

gcWSSize :: Real
gcWSSize = 10.0

gcPlayerHeight :: Real
gcPlayerHeight = 20.0

gcGrv :: Real
gcGrv = 0.21875

gcTimeMultiplier :: Real
gcTimeMultiplier = 30.0

gcPlayerMaxSpeed :: Real
gcPlayerMaxSpeed = 6.0

gcAcc :: Real
gcAcc = 0.046875

gcAir :: Real
gcAir = 2.0 * gcAcc

gcJmp :: Real
gcJmp = -6.5

gcFrc :: Real
gcFrc = gcAcc

gcDec :: Real
gcDec = 0.5

gcTileSize :: Int
gcTileSize = 35
