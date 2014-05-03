module Jumpie.Game(processGame) where

import Prelude(Double,(+),(-),(*),abs,signum)
import Data.Ord((<),(>=),(>),min)
import Data.Bool((&&),(||),not)
import Jumpie.Geometry.Point(Point2(..))
import Jumpie.Geometry.Utility(clampAbs)
import Jumpie.Types(TimeDelta,GameState,IncomingAction(..),GameObject(..),Player(Player),playerMode,PlayerMode(..),LineSegmentReal,box,PointReal,Real,isBox,SensorLine(SensorLine),playerPosition,Box(Box),timeDelta,playerVelocity,FrameState,getTimeDelta)
import Jumpie.Geometry.Rect(top,center,right,left)
import Control.Applicative((<|>))
import Data.Maybe(Maybe(..),isNothing,isJust)
import Jumpie.Geometry.LineSegment(LineSegment(LineSegment))
import Data.List(concatMap,map,filter,(++),elem)
import Jumpie.Maybe(ifMaybe)
import Jumpie.Geometry.Intersection(rectLineSegmentIntersects)
import Data.Monoid(First(First),getFirst,mconcat)
import Data.Function((.),($))
import Jumpie.GameConfig(gcPlayerMaxSpeed,gcAcc,gcFrc,gcWSSize, gcPlayerHeight, gcGrv,gcDec,gcAir,gcJmp)

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
  getWCollision :: Maybe GameObject,
  getFLCollision :: Maybe GameObject,
  getFRCollision :: Maybe GameObject
  }

applySensors :: [GameObject] -> PointReal -> Real -> Sensors
applySensors go p wSDev = Sensors wS fSL fSR wSCollision fSLCollision fSRCollision
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
        fSL = LineSegment (Point2 fSLX fSYTop) (Point2 fSLX fSYBottom)
        fSR = LineSegment (Point2 fSRX fSYTop) (Point2 fSRX fSYBottom)
        fSLCollision = lineCollision boxes fSL
        fSRCollision = lineCollision boxes fSR

processGroundPlayerObject :: FrameState -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processGroundPlayerObject fs os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map (ObjectSensorLine . SensorLine) [getW sensors,getFL sensors,getFR sensors]
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
          playerVelocity = newPlayerVelocity
          }

processAirPlayerObject :: FrameState -> [GameObject] -> [IncomingAction] -> Player -> [GameObject]
processAirPlayerObject fs os ias p = [ObjectPlayer np] ++ sensorLines
  where sensorLines = map (ObjectSensorLine . SensorLine) [getW sensors,getFL sensors,getFR sensors]
        t = getTimeDelta fs
        sensors = applySensors os (playerPosition p) 0.0
        fCollision = getFLCollision sensors <|> getFRCollision sensors
        movingDownwards = (_y . playerVelocity) p >= 0.0
        -- Ist der naechste Zustand der Bodenzustand?
        newPlayerMode = case fCollision of
          Just (ObjectBox (Box r)) -> if movingDownwards && (_y . playerPosition) p + gcPlayerHeight > top r
                        then Ground
                        else Air
          _ -> Air
        newPlayerPositionX = case (getWCollision sensors) of
          Just (ObjectBox (Box r)) -> if (_x . center) r < (_x . playerPosition) p
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          _ -> (_x . playerPosition) p + timeDelta t * (_x . playerVelocity) p
        newPlayerPositionY = (_y . playerPosition) p + timeDelta t * (_y . playerVelocity) p
        playerAcc = if PlayerLeft `elem` ias
                    then Just (-gcAir)
                    else
                         if PlayerRight `elem` ias
                         then Just gcAir
                         else Nothing
        oldPlayerVelocityX = (_x  . playerVelocity) p
        newPlayerVelocityX = if isJust (getWCollision sensors)
                             then 0.0
                             else case playerAcc of
                               Just v -> clampAbs gcPlayerMaxSpeed $ oldPlayerVelocityX + v
                               Nothing -> airDrag oldPlayerVelocityX
        oldPlayerVelocityY = (_y . playerVelocity) p
        newPlayerVelocityY = if oldPlayerVelocityY < -4.0 && not (PlayerJump `elem` ias)
                             then -4.0
                             else oldPlayerVelocityY + timeDelta t * gcGrv
        airDrag xv = if newPlayerVelocityY < 0.0 && newPlayerVelocityY > -4.0 && abs xv >= 0.125 then xv * 0.9685 else xv
        np = Player {
          playerPosition = Point2 newPlayerPositionX newPlayerPositionY,
          playerMode = newPlayerMode,
          playerVelocity = Point2 newPlayerVelocityX newPlayerVelocityY
          }
