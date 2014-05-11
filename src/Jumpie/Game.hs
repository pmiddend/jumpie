module Jumpie.Game(processGame) where

import Control.Applicative((<|>))
import Data.Bool((&&),(||),not,Bool(False))
import Data.Eq((==))
import Data.Function((.),($))
import Data.List(concatMap,map,filter,(++),elem)
import Data.Maybe(Maybe(..),isNothing,isJust,fromJust)
import Data.Monoid(First(First),getFirst,mconcat)
import Data.Ord((<),(>=),(>),min)
import Jumpie.GameConfig(gcPlayerMaxSpeed,gcAcc,gcFrc,gcWSSize, gcPlayerHeight, gcGrv,gcDec,gcAir,gcJmp)
import Jumpie.Geometry.Intersection(rectLineSegmentIntersects)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Jumpie.Geometry.Point(Point2(..))
import Jumpie.Geometry.Rect(top,center,right,left,bottom)
import Jumpie.Geometry.Utility(clampAbs)
import Jumpie.Maybe(ifMaybe)
import Jumpie.Types(TimeDelta,GameState,IncomingAction(..),GameObject(..),Player(Player),playerMode,PlayerMode(..),LineSegmentReal,box,PointReal,Real,isBox,SensorLine(SensorLine),playerPosition,Box(Box),timeDelta,playerVelocity,FrameState,getTimeDelta,playerWalkSince,getCurrentTicks)
import Prelude(Double,(+),(-),(*),abs,signum)

processGame :: FrameState -> GameState -> [IncomingAction] -> GameState
processGame fs gameObjects actions = concatMap (processGameObject fs gameObjects actions) gameObjects

processGameObject :: FrameState -> [GameObject] -> [IncomingAction] -> GameObject -> [GameObject]
processGameObject fs os ias o = case o of
  ObjectPlayer p -> processPlayerObject fs os ias p
  ObjectBox b -> [ObjectBox b]
  ObjectSensorLine _ -> []

processPlayerObject :: FrameState -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processPlayerObject fs os ias p = case playerMode p of
  Ground -> processGroundPlayerObject fs os ias p
  Air -> processAirPlayerObject fs os ias p

-- Testet, ob ein LineSegment (ein Sensor) mit der Umgebung kollidiert
lineCollision :: [GameObject] -> LineSegmentReal -> Maybe GameObject
lineCollision boxes l = (getFirst . mconcat . map (First . (collides l))) boxes
  where collides :: LineSegmentReal -> GameObject -> Maybe GameObject
        collides l1 bp@(ObjectBox b) = ifMaybe (rectLineSegmentIntersects 0.01 (box b) l1) bp
        collides _ _ = Nothing

data Sensors = Sensors {
  getW :: LineSegment PointReal,
  getFL :: LineSegment PointReal,
  getFR :: LineSegment PointReal,
  getCL :: LineSegment PointReal,
  getCR :: LineSegment PointReal,
  getWCollision :: Maybe GameObject,
  getFLCollision :: Maybe GameObject,
  getFRCollision :: Maybe GameObject,
  getCLCollision :: Maybe GameObject,
  getCRCollision :: Maybe GameObject
  }

applySensors :: [GameObject] -> PointReal -> Real -> Sensors
applySensors go p wSDev = Sensors wS fSL fSR cSL cSR wSCollision fSLCollision fSRCollision cSLCollision cSRCollision
  where boxes = filter isBox go
        wSCollision = lineCollision boxes wS
        wS = LineSegment (Point2 wSL wSY) (Point2 wSR wSY)
        wSY = _y p + wSDev
        wSL = _x p - gcWSSize
        wSR = _x p + gcWSSize
        fSLX = _x p - 9.0
        fSRX = _x p + 9.0
        fSYTop = _y p + gcPlayerHeight
        fSYBottom = _y p + gcPlayerHeight + 16.0
        cSYTop = _y p - gcPlayerHeight
        cSYBottom = _y p - gcPlayerHeight - 16.0
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
  where sensorLines = map (ObjectSensorLine . SensorLine) [getW sensors,getFL sensors,getFR sensors,getCL sensors,getCR sensors]
        sensors = applySensors os (playerPosition p) 4.0
        t = getTimeDelta fs
        fCollision = getFLCollision sensors <|> getFRCollision sensors
        newPlayerMode = if isNothing fCollision || PlayerJump `elem` ias
                        then Air
                        else Ground
        newPlayerPositionX = case getWCollision sensors of
          Just (ObjectBox (Box r)) -> if (_x . center) r < (_x . playerPosition) p
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          _ -> (_x . playerPosition) p + timeDelta t * (_x . playerVelocity) p
        newPlayerPositionY = case fCollision of
          Just (ObjectBox (Box r)) -> top r - gcPlayerHeight
          _ -> (_y . playerPosition) p
        newPlayerVelocityY = if PlayerJump `elem` ias then gcJmp else 0.0
        oldPlayerVelocityX = (_x  . playerVelocity) p
        playerAcc = if PlayerLeft `elem` ias
                    then Just $ if oldPlayerVelocityX > 0.0 then -gcDec else (-gcAcc)
                    else
                         if PlayerRight `elem` ias
                         then Just $ if oldPlayerVelocityX < 0.0 then gcDec else gcAcc
                         else Nothing
        newPlayerVelocityX = case playerAcc of
          Just v -> clampAbs gcPlayerMaxSpeed $ oldPlayerVelocityX + v
          Nothing -> oldPlayerVelocityX - min (abs oldPlayerVelocityX) gcFrc * signum oldPlayerVelocityX
        newPlayerVelocity = if isJust (getWCollision sensors)
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
  where sensorLines = map (ObjectSensorLine . SensorLine) [getW sensors,getFL sensors,getFR sensors,getCL sensors,getCR sensors]
        t = getTimeDelta fs
        sensors = applySensors os (playerPosition p) 0.0
        fCollision = getFLCollision sensors <|> getFRCollision sensors
        cCollision = getCLCollision sensors <|> getCRCollision sensors
        movingDownwards = (_y . playerVelocity) p >= 0.0
        oldPlayerPositionX = (_x . playerPosition) p
        oldPlayerPositionY = (_y . playerPosition) p
        oldPlayerVelocityX = (_x  . playerVelocity) p
        oldPlayerVelocityY = (_y . playerVelocity) p
        playerIsAboveBox b = case b of
          (ObjectBox (Box r)) -> oldPlayerPositionY - gcPlayerHeight < bottom r
          _ -> False
        playerIsBelowBox b = case b of
          (ObjectBox (Box r)) -> oldPlayerPositionY + gcPlayerHeight > top r
          _ -> False
        -- Ist der naechste Zustand der Bodenzustand?
        newPlayerMode = case fCollision of
          Just b -> if movingDownwards && playerIsBelowBox b
                    then Ground
                    else Air
          _ -> Air
        newPlayerPositionX = case getWCollision sensors of
          Just (ObjectBox (Box r)) -> if (_x . center) r < oldPlayerPositionX
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          _ -> oldPlayerPositionX + timeDelta t * oldPlayerVelocityX
        newPlayerPositionY = case cCollision of
          Just b@(ObjectBox (Box r)) -> if oldPlayerVelocityY < 0.0 && playerIsAboveBox b
                                        then bottom r + gcPlayerHeight
                                        else oldPlayerPositionY + timeDelta t * oldPlayerVelocityY
          _ -> oldPlayerPositionY + timeDelta t * oldPlayerVelocityY
        playerAcc = if PlayerLeft `elem` ias
                    then Just (-gcAir)
                    else
                         if PlayerRight `elem` ias
                         then Just gcAir
                         else Nothing
        newPlayerVelocityX = if isJust (getWCollision sensors)
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
          playerWalkSince = if newPlayerMode == Ground then Just (getCurrentTicks fs) else Nothing
          }
