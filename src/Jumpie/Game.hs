module Jumpie.Game(processGameObjects,testGameOver) where

import           Control.Monad.State.Strict   (gets)
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  (First (First), getFirst)
import Wrench.Time
import           Jumpie.GameConfig            (gcAcc, gcAir, gcDec, gcFrc,
                                               gcGrv, gcJmp, gcPlayerHeight,
                                               gcPlayerMaxSpeed, gcStarLifetime,
                                               gcWSSize, screenHeight,gcStarCollisionDistance)
import           Jumpie.GameData              (GameDataM, gdCurrentTicks,
                                               gdTimeDelta)
import           Jumpie.GameObject            (Box (Box), GameObject (..),
                                               Player (Player), PlayerMode (..),
                                               SensorLine (SensorLine),
                                               boxPosition, isBox, isPlayer,
                                               playerMode, playerPosition,
                                               playerVelocity, playerWalkSince,
                                               starInception,Star,starPosition)
import           Jumpie.GameState             (GameState, gsObjects)
import           Jumpie.Geometry.Intersection (rectLineSegmentIntersects)
import           Jumpie.Geometry.LineSegment  (LineSegment (LineSegment))
import           Jumpie.Geometry.Point        (Point2 (..),euclideanDistance)
import           Jumpie.Geometry.Rect         (bottom, center, left, right, top)
import           Jumpie.Geometry.Utility      (clampAbs)
import           Jumpie.Maybe                 (ifMaybe)
import           Jumpie.Types                 (IncomingAction (..),
                                               LineSegmentReal, PointReal, Real,OutgoingAction(..))
import ClassyPrelude hiding(Real)

processGameObjects :: GameState -> [IncomingAction] -> GameDataM p ([GameObject],[OutgoingAction])
processGameObjects gs actions = do
  objectsnactions <- mapM (processGameObject gs actions) (gsObjects gs)
  return . (bimap concat concat) . unzip $ objectsnactions

testGameOver :: GameState -> Bool
testGameOver os = case find isPlayer (gsObjects os) of
  Nothing -> True
  Just (ObjectPlayer p) -> (pY . playerPosition) p > fromIntegral screenHeight
  Just _ -> True

processGameObject :: GameState -> [IncomingAction] -> GameObject -> GameDataM p ([GameObject],[OutgoingAction])
processGameObject gs ias o = case o of
  ObjectPlayer p -> processPlayerObject gs ias p
  ObjectBox b -> return ([ObjectBox b],[])
  ObjectStar b -> processStar gs b
  ObjectSensorLine _ -> return ([],[])

processStar :: GameState -> Star -> GameDataM p ([GameObject],[OutgoingAction])
processStar gs b = do
  currentTicks <- gets gdCurrentTicks
  -- Stern abgelaufen?
  if starInception b `plusDuration` gcStarLifetime < currentTicks
    then return ([],[])
    -- Stern kollidiert mit Spieler?
    else case find isPlayer (gsObjects gs) of
      Nothing -> return ([ObjectStar b],[])
      Just (ObjectPlayer p) -> if playerPosition p `euclideanDistance` starPosition b < gcStarCollisionDistance
                then return ([],[StarCollected])
                else return ([ObjectStar b],[])
      Just _ -> error "Player is not a player, wtf?"

processPlayerObject :: GameState -> [IncomingAction] -> Player -> GameDataM p ([GameObject],[OutgoingAction])
processPlayerObject gs ias p = case playerMode p of
  Ground -> processGroundPlayerObject gs ias p
  Air -> processAirPlayerObject gs ias p

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

processGroundPlayerObject :: GameState -> [IncomingAction] -> Player -> GameDataM p ([GameObject],[OutgoingAction])
processGroundPlayerObject gs ias p = do
  t <- gets gdTimeDelta
  let   sensorLines = map (ObjectSensorLine . SensorLine) [sensW sensors,sensFL sensors,sensFR sensors,sensCL sensors,sensCR sensors]
        sensors = applySensors (gsObjects gs) (playerPosition p) 4.0
        fCollision = sensFLCollision sensors <|> sensFRCollision sensors
        newPlayerMode = if isNothing fCollision || PlayerJump `elem` ias
                        then Air
                        else Ground
        newPlayerPositionX = case sensWCollision sensors of
          Just (ObjectBox (Box r _)) -> if (pX . center) r < (pX . playerPosition) p
                                     then (right r) + gcWSSize + 1.0
                                     else (left r) - (gcWSSize + 1.0)
          _ -> (pX . playerPosition) p + toSeconds t * (pX . playerVelocity) p
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
  return $ ([ObjectPlayer np] ++ sensorLines,[])

processAirPlayerObject :: GameState -> [IncomingAction] -> Player -> GameDataM p ([GameObject],[OutgoingAction])
processAirPlayerObject gs ias p = do
  t <- gets gdTimeDelta
  currentTicks <- gets gdCurrentTicks
  let   sensorLines = map (ObjectSensorLine . SensorLine) [sensW sensors,sensFL sensors,sensFR sensors,sensCL sensors,sensCR sensors]
        sensors = applySensors (gsObjects gs) (playerPosition p) 0.0
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
          _ -> oldPlayerPositionX + toSeconds t * oldPlayerVelocityX
        newPlayerPositionY = case cCollision of
          Just b@(ObjectBox (Box r _)) -> if oldPlayerVelocityY < 0.0 && playerIsAboveBox b
                                        then bottom r + gcPlayerHeight
                                        else oldPlayerPositionY + toSeconds t * oldPlayerVelocityY
          _ -> oldPlayerPositionY + toSeconds t * oldPlayerVelocityY
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
                             then toSeconds t * gcGrv
                             else if oldPlayerVelocityY < -4.0 && not (PlayerJump `elem` ias)
                                  then -4.0
                                  else oldPlayerVelocityY + toSeconds t * gcGrv
        airDrag xv = if newPlayerVelocityY < 0.0 && newPlayerVelocityY > -4.0 && abs xv >= 0.125
                     then xv * 0.9685
                     else xv
        np = Player {
          playerPosition = Point2 newPlayerPositionX newPlayerPositionY,
          playerMode = newPlayerMode,
          playerVelocity = Point2 newPlayerVelocityX newPlayerVelocityY,
          playerWalkSince = if newPlayerMode == Ground then Just currentTicks else Nothing
          }
  return $ ([ObjectPlayer np] ++ sensorLines,[])
