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
  gcTimeMultiplier,
  initialGameState,
  screenWidth,
  screenHeight,
  screenBpp,
  mediaDir
  ) where

import Jumpie.GameObject(Player(Player),playerPosition,playerWalkSince,playerMode,playerVelocity,PlayerMode(..),Box(Box),GameObject(..))
import Jumpie.Types(Real)
import Jumpie.Geometry.Point(Point2(Point2))
import Jumpie.Geometry.Rect(Rect(Rect),rectBottomRight,rectTopLeft)
import Prelude(fromIntegral,(/),(-),div,(*),(+))
import Data.Function(($))
import Control.Applicative((<$>))
import Data.Maybe(Maybe(Nothing))
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

gcTileSize :: Real
gcTileSize = 40.0

initialPlayer :: Player
initialPlayer = Player {
  playerPosition = Point2 (fromIntegral screenWidth / 2.0) (fromIntegral screenHeight / 4.0),
  playerMode = Air,
  playerVelocity = Point2 0.0 0.0,
  playerWalkSince = Nothing
  }

initialBoxes :: [Box]
initialBoxes = map (\x -> toBox x yBaseline) [0..boxesPerScreen-1] ++ otherBoxes
  where rectSize = 35
        yBaseline = fromIntegral $ screenHeight `div` 2 - rectSize `div` 2
        boxesPerScreen = screenWidth `div` rectSize
        otherBoxes = [toBox 2 (yBaseline - fromIntegral rectSize),toBox 10 (yBaseline - 3.0 * fromIntegral rectSize)]
        toBox xRaw yRaw = Box $ Rect {
          rectTopLeft = Point2 (fromIntegral (xRaw*rectSize)) yRaw,
          rectBottomRight = Point2 (fromIntegral ((xRaw+1)*rectSize)) (yRaw + fromIntegral rectSize)
          }

initialGameState :: [GameObject]
initialGameState = ObjectPlayer initialPlayer : (ObjectBox <$> initialBoxes)
