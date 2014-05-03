module Jumpie.GameConfig(
  gcWSSize,
  gcPlayerHeight,
  gcPlayerMaxSpeed,
  gcDec,
  gcAir,
  gcGrv,
  gcAcc,
  gcJmp,
  gcFrc,
  gcTimeMultiplier,
  initialGameState,
  screenWidth,
  screenHeight,
  screenBpp,
  mediaDir
  ) where

import Jumpie.Types(Real,Player(Player),playerPosition,playerMode,playerVelocity,PlayerMode(..),Box(Box),GameObject(..))
import Jumpie.Geometry.Point(Point2(Point2))
import Jumpie.Geometry.Rect(Rect(Rect),bottomRight,topLeft)
import Prelude(fromIntegral,(/),(-),div,(*),(+))
import Data.Function(($))
import Control.Applicative((<$>))
import Data.List(map,(++))
import Data.Int(Int)
import Data.String(String)

screenWidth,screenHeight,screenBpp :: Int
screenWidth = 800
screenHeight = 600
screenBpp = 32
mediaDir :: String
mediaDir = "media"

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

initialPlayer :: Player
initialPlayer = Player {
  playerPosition = Point2 (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 4.0),
  playerMode = Air,
  playerVelocity = Point2 0.0 0.0
  }

initialBoxes :: [Box]
initialBoxes = map (\x -> toBox x yBaseline) [0..boxesPerScreen-1] ++ otherBoxes
  where rectSize = 35
        yBaseline = fromIntegral $ screenHeight `div` 2 - rectSize `div` 2
        boxesPerScreen = screenWidth `div` rectSize
        otherBoxes = [toBox 2 (yBaseline - fromIntegral rectSize)]
        toBox xRaw yRaw = Box $ Rect {
          topLeft = Point2 (fromIntegral (xRaw*rectSize)) yRaw,
          bottomRight = Point2 (fromIntegral ((xRaw+1)*rectSize)) (yRaw + fromIntegral rectSize)
          }

initialGameState :: [GameObject]
initialGameState = ObjectPlayer initialPlayer : (ObjectBox <$> initialBoxes)
