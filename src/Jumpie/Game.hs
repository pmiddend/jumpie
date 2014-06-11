module Jumpie.Game(processGameObjects) where

import           Control.Applicative          ((<|>))
import           Data.Bool                    (Bool (False, True), not, (&&),
                                               (||))
import           Data.Eq                      ((==))
import           Data.Function                (($), (.))
import           Data.List                    (concatMap, elem, filter, find,
                                               map, (++))
import           Data.Maybe                   (Maybe (..), fromJust, isJust,
                                               isNothing)
import           Data.Monoid                  (First (First), getFirst, mconcat)
import           Data.Ord                     (min, (<), (>), (>=))
import           Jumpie.FrameState            (FrameState, fsCurrentTicks,
                                               fsTimeDelta)
import           Jumpie.GameConfig            (gcAcc, gcAir, gcDec, gcFrc,
                                               gcGrv, gcJmp, gcPlayerHeight,
                                               gcPlayerMaxSpeed, gcStarLifetime,
                                               gcWSSize, screenHeight)
import           Jumpie.GameObject            (Box (Box), GameObject (..),
                                               Player (Player), PlayerMode (..),
                                               SensorLine (SensorLine),
                                               boxPosition, isBox, isPlayer,
                                               playerMode, playerPosition,
                                               playerVelocity, playerWalkSince,starInception)
import           Jumpie.GameState             (GameState (GameState), gsObjects)
import           Jumpie.Geometry.Intersection (rectLineSegmentIntersects)
import           Jumpie.Geometry.LineSegment  (LineSegment (LineSegment))
import           Jumpie.Geometry.Point        (Point2 (..))
import           Jumpie.Geometry.Rect         (bottom, center, left, right, top)
import           Jumpie.Geometry.Utility      (clampAbs)
import           Jumpie.Maybe                 (ifMaybe)
import           Jumpie.Time                  (TimeDelta, timeDelta)
import           Jumpie.Types                 (IncomingAction (..),
                                               LineSegmentReal, PointReal, Real)
import           Prelude                      (Double, abs, fromIntegral,
                                               signum, (*), (+), (-))

processGameObjects :: FrameState -> GameState -> [IncomingAction] -> GameState
processGameObjects fs gs actions = GameState newGameObjects (testGameOver newGameObjects)
  where newGameObjects = concatMap (processGameObject fs (gsObjects gs) actions) (gsObjects gs)

testGameOver :: [GameObject] -> Bool
testGameOver os = case find isPlayer os of
  Nothing -> True
  Just (ObjectPlayer p) -> (pY . playerPosition) p > fromIntegral screenHeight
  Just _ -> True

processGameObject :: FrameState -> [GameObject] -> [IncomingAction] -> GameObject -> [GameObject]
processGameObject fs os ias o = case o of
  ObjectPlayer p -> processPlayerObject fs os ias p
  ObjectBox b -> [ObjectBox b]
  ObjectStar b -> if starInception b + gcStarLifetime > (fsCurrentTicks fs)
                  then []
                  else [ObjectStar b]
  ObjectSensorLine _ -> []

processPlayerObject :: FrameState -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processPlayerObject fs os ias p = case playerMode p of
  Ground -> processGroundPlayerObject fs os ias p
  Air -> processAirPlayerObject fs os ias p

-- Testet, ob ein LineSegment (ein Sensor) mit der Umgebung kollidiert
lineCollision :: [GameObject] -> LineSegmentReal -> Maybe GameObject
lineCollision boxes l = (getFirst . mconcat . map (First . (collides l))) boxes
  where collides :: LineSegmentReal -> GameObject -> Maybe GameObject
        collides l1 bp@(ObjectBox b) = ifMaybe (rectLineSegmentIntersects 0.01 (boxPosition b) l1) bp
        collides _ _ = Nothing

data Sensors = Sensors {
  sensW           :: LineSegment PointReal,
  sensFL          :: LineSegment PointReal,
  sensFR          :: LineSegment PointReal,
  sensCL          :: LineSegment PointReal,
  sensCR          :: LineSegment PointReal,
  sensWCollision  :: Maybe GameObject,
  sensFLCollision :: Maybe GameObject,
  sensFRCollision :: Maybe GameObject,
  sensCLCollision :: Maybe GameObject,
  sensCRCollision :: Maybe GameObject
  }

applySensors :: [GameObject] -> PointReal -> Real -> Sensors
applySensors go p wSDev = Sensors wS fSL fSR cSL cSR wSCollision fSLCollision fSRCollision cSLCollision cSRCollision
  where boxes = filter isBox go
        wSCollision = lineCollision boxes wS
        wS = LineSegment (Point2 wSL wSY) (Point2 wSR wSY)
        wSY = pY p + wSDev
        wSL = pX p - gcWSSize
        wSR = pX p + gcWSSize
        fSLX = pX p - 9.0
        fSRX = pX p + 9.0
        fSYTop = pY p + gcPlayerHeight
        fSYBottom = pY p + gcPlayerHeight + 16.0
        cSYTop = pY p - gcPlayerHeight
        cSYBottom = pY p - gcPlayerHeight - 16.0
        fSL = LineSegment (Point2 fSLX fSYTop) (Point2 fSLX fSYBottom)
        fSR = LineSegment (Point2 fSRX fSYTop) (Point2 fSRX fSYBottom)
        cSL = LineSegment (Point2 fSLX cSYTop) (Point2 fSLX cSYBottom)
        cSR = LineSegment (Point2 fSRX cSYTop) (Point2 fSRX cSYBottom)
        fSLCollision = lineCollision boxes fSL
        fSRCollision = lineCollision boxes fSR
        cSLCollision = lineCollision boxes cSL
        cSRCollision = lineCollision boxes cSR

processGroundPlayerObject :: FrameState -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processGroundPlayerObject fs os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map (ObjectSensorLine . SensorLine) [sensW sensors,sensFL sensors,sensFR sensors,sensCL sensors,sensCR sensors]
        sensors = applySensors os (playerPosition p) 4.0
        t = fsTimeDelta fs
        fCollision = sensFLCollision sensors <|> sensFRCollision sensors
        newPlayerMode = if isNothing fCollision || PlayerJump `elem` ias
                        then Air
                        else Ground
        newPlayerPositionX = case sensWCollision sensors of
          Just (ObjectBox (Box r _)) -> if (pX . center) r < (pX . playerPosition) p
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          _ -> (pX . playerPosition) p + timeDelta t * (pX . playerVelocity) p
        newPlayerPositionY = case fCollision of
          Just (ObjectBox (Box r _)) -> top r - gcPlayerHeight
          _ -> (pY . playerPosition) p
        newPlayerVelocityY = if PlayerJump `elem` ias then gcJmp else 0.0
        oldPlayerVelocityX = (pX  . playerVelocity) p
        playerAcc = if PlayerLeft `elem` ias
                    then Just $ if oldPlayerVelocityX > 0.0 then -gcDec else (-gcAcc)
                    else
                         if PlayerRight `elem` ias
                         then Just $ if oldPlayerVelocityX < 0.0 then gcDec else gcAcc
                         else Nothing
        newPlayerVelocityX = case playerAcc of
          Just v -> clampAbs gcPlayerMaxSpeed $ oldPlayerVelocityX + v
          Nothing -> oldPlayerVelocityX - min (abs oldPlayerVelocityX) gcFrc * signum oldPlayerVelocityX
        newPlayerVelocity = if isJust (sensWCollision sensors)
                            then Point2 0.0 0.0
                            else Point2 newPlayerVelocityX newPlayerVelocityY
        np = Player {
          playerPosition = Point2 newPlayerPositionX newPlayerPositionY,
          playerMode = newPlayerMode,
          playerVelocity = newPlayerVelocity,
          playerWalkSince = if newPlayerMode == Ground then (playerWalkSince p) else Nothing
          }

processAirPlayerObject :: FrameState -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processAirPlayerObject fs os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map (ObjectSensorLine . SensorLine) [sensW sensors,sensFL sensors,sensFR sensors,sensCL sensors,sensCR sensors]
        t = fsTimeDelta fs
        sensors = applySensors os (playerPosition p) 0.0
        fCollision = sensFLCollision sensors <|> sensFRCollision sensors
        cCollision = sensCLCollision sensors <|> sensCRCollision sensors
        movingDownwards = (pY . playerVelocity) p >= 0.0
        oldPlayerPositionX = (pX . playerPosition) p
        oldPlayerPositionY = (pY . playerPosition) p
        oldPlayerVelocityX = (pX  . playerVelocity) p
        oldPlayerVelocityY = (pY . playerVelocity) p
        playerIsAboveBox b = case b of
          (ObjectBox (Box r _)) -> oldPlayerPositionY - gcPlayerHeight < bottom r
          _ -> False
        playerIsBelowBox b = case b of
          (ObjectBox (Box r _)) -> oldPlayerPositionY + gcPlayerHeight > top r
          _ -> False
        -- Ist der naechste Zustand der Bodenzustand?
        newPlayerMode = case fCollision of
          Just b -> if movingDownwards && playerIsBelowBox b
                    then Ground
                    else Air
          _ -> Air
        newPlayerPositionX = case sensWCollision sensors of
          Just (ObjectBox (Box r _)) -> if (pX . center) r < oldPlayerPositionX
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          _ -> oldPlayerPositionX + timeDelta t * oldPlayerVelocityX
        newPlayerPositionY = case cCollision of
          Just b@(ObjectBox (Box r _)) -> if oldPlayerVelocityY < 0.0 && playerIsAboveBox b
                                        then bottom r + gcPlayerHeight
                                        else oldPlayerPositionY + timeDelta t * oldPlayerVelocityY
          _ -> oldPlayerPositionY + timeDelta t * oldPlayerVelocityY
        playerAcc = if PlayerLeft `elem` ias
                    then Just (-gcAir)
                    else
                         if PlayerRight `elem` ias
                         then Just gcAir
                         else Nothing
        newPlayerVelocityX = if isJust (sensWCollision sensors)
                             then 0.0
                             else case playerAcc of
                               Just v -> clampAbs gcPlayerMaxSpeed $ oldPlayerVelocityX + v
                               Nothing -> airDrag oldPlayerVelocityX
        newPlayerVelocityY = if oldPlayerVelocityY < 0.0 && isJust cCollision && playerIsAboveBox (fromJust cCollision)
                             then timeDelta t * gcGrv
                             else if oldPlayerVelocityY < -4.0 && not (PlayerJump `elem` ias)
                                  then -4.0
                                  else oldPlayerVelocityY + timeDelta t * gcGrv
        airDrag xv = if newPlayerVelocityY < 0.0 && newPlayerVelocityY > -4.0 && abs xv >= 0.125
                     then xv * 0.9685
                     else xv
        np = Player {
          playerPosition = Point2 newPlayerPositionX newPlayerPositionY,
          playerMode = newPlayerMode,
          playerVelocity = Point2 newPlayerVelocityX newPlayerVelocityY,
          playerWalkSince = if newPlayerMode == Ground then Just (fsCurrentTicks fs) else Nothing
          }
