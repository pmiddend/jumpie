module Jumpie.GameConfig(
  gcWSSize,
  gcTileSize,
  gcPlayerHeight,
  gcPlayerMaxSpeed,
  gcStars,
  gcDec,
  gcStarCollisionDistance,
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
  mediaDir,
  backgroundColor,
  gcStarLifetime,
  gcStarWiggleHeight,
  gcStarWiggleSpeed,
  gcAudioChunkSize
  ) where

import Wrench.Time
import Wrench.Color
import           Jumpie.Types (Real)
import ClassyPrelude hiding(Real)

backgroundColor :: Color
backgroundColor = mkColorFromRgba 94 129 162 255

screenWidth,screenHeight :: Int
screenWidth = 1200
screenHeight = 400
mediaDir :: String
mediaDir = "media"

gcPlatMaxLength :: Int
gcPlatMaxLength = 5

gcStarWiggleHeight :: Real
gcStarWiggleHeight = 5

gcStarWiggleSpeed :: Real
gcStarWiggleSpeed = 10

gcPlatCount :: Int
gcPlatCount = 15

gcStars :: Int
gcStars = 3

gcStarCollisionDistance :: Real
gcStarCollisionDistance = 17

gcWSSize :: Real
gcWSSize = 10.0

gcPlayerHeight :: Real
gcPlayerHeight = 20.0

gcGrv :: Real
-- Sonic-Wert
gcGrv = 0.21875

gcTimeMultiplier :: Real
--gcTimeMultiplier = 30.0
gcTimeMultiplier = 50.0

gcPlayerMaxSpeed :: Real
gcPlayerMaxSpeed = 6.0

gcAcc :: Real
-- Sonic-Wert
--gcAcc = 0.046875
gcAcc = 0.046875

gcAir :: Real
gcAir = gcAcc
--gcAir = gcAcc

gcJmp :: Real
gcJmp = -6.5

gcFrc :: Real
gcFrc = gcAcc

gcDec :: Real
gcDec = 0.5

gcTileSize :: Int
gcTileSize = 35

gcStarLifetime :: TimeDelta
gcStarLifetime = fromSeconds 3

gcAudioChunkSize :: Int
gcAudioChunkSize = 1024
