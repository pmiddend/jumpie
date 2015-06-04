{-# LANGUAGE OverloadedStrings #-}
module Jumpie.Game(processGameObjects,testGameOver) where

import           Data.Maybe                   (fromJust)
import           Data.Monoid                  (First (First), getFirst)
import Wrench.Time
import           Jumpie.GameConfig
import           Jumpie.MonadGame
import           Jumpie.GameObject
import           Jumpie.GameState
import           Jumpie.GameGeneration
import           Jumpie.Geometry.Intersection (rectLineSegmentIntersects)
import           Jumpie.Geometry.LineSegment  (LineSegment (LineSegment))
import           Jumpie.Geometry.Rect         (bottom, center, left, right, top)
import           Jumpie.Geometry.Utility      (clampAbs)
import Jumpie.Sensors
import           Jumpie.Maybe                 (ifMaybe)
import           Jumpie.Types                 (IncomingAction (..),
                                               LineSegmentReal, PointReal, Real,OutgoingAction(..))
import ClassyPrelude hiding(Real,head,last)
import Linear.V2(_x,_y,V2(..))
import Control.Lens((^.))
import Wrench.Animation
import Data.List(tail,head,last)
import Control.Lens(_1,_2,view)
import Control.Monad.Random(MonadRandom)

processGameObjects :: (MonadRandom m,Monad m,MonadIO m,Applicative m,MonadGame m) => GameState -> [IncomingAction] -> m (Player,PointReal,WorldSection,[WorldSection],[OutgoingAction])
processGameObjects gs actions = do
  -- [(WorldSection,[OutgoingAction])]
  sectionsWithActions <- traverse (processWorldSection gs actions) (gs ^. gsSections)
  (newTempSection,tempSectionActions) <- processWorldSection gs actions (gs ^. gsTempSection)
  ticks <- gcurrentTicks
  let
    sections = view _1 <$> sectionsWithActions
    secActions = join ((view _2) <$> sectionsWithActions)
  (newPlayer,playerObjects,playerActions) <- processPlayerObject gs actions (gs ^. gsPlayer) 
  let
    firstSection = head sections
    tailSections = tail sections
    totalActions = tempSectionActions <> playerActions <> secActions
  case firstSection of
    [] -> do
      putStrLn "Generating new section"
      let
        newFirstSection = head tailSections
        lastSection = last tailSections
        (newFirstSectionStart,_) = sectionWidth newFirstSection
        (_,lastSectionEnd) = sectionWidth lastSection
        newTempSectionMoved = moveSection newFirstSectionStart newTempSection
        movedTails = (moveSection newFirstSectionStart) <$> tailSections
      newSection <- moveSection (lastSectionEnd-newFirstSectionStart+fromIntegral gcTileSize) <$> (generateSection ticks)
      -- TODO: camera position only changes x value - better lens here
      return (newPlayer,V2 (gs ^. gsCameraPosition ^. _x - newFirstSectionStart) (gs ^. gsCameraPosition  ^. _y),newTempSectionMoved,(movedTails ++ [newSection]),totalActions)
    xs -> do
      return (newPlayer,gs ^. gsCameraPosition,newTempSection <> playerObjects,xs : tailSections,totalActions)
  

processWorldSection :: (Monad m,Applicative m,MonadGame m) => GameState -> [IncomingAction] -> WorldSection -> m (WorldSection,[OutgoingAction])
processWorldSection gs actions section = do
  objectsnactions <- traverse (processGameObject gs actions) section
  let
    (newObjects,objectActions) = bimap concat concat . unzip $ objectsnactions
  return (newObjects,objectActions)

processGameObject :: (Functor m,MonadGame m,Monad m) => GameState -> [IncomingAction] -> GameObject -> m ([GameObject],[OutgoingAction])
processGameObject gs _ o = case o of
  ObjectPlayer _ -> error "player given to processGameObject, check the code"
  ObjectBox b -> do
    ticks <- gcurrentTicks
    if ticks > b ^. boxDeadline
      then return ([],[])
      else return ([ObjectBox b],[])
  ObjectParticle b -> processParticle gs b
  ObjectSensorLine _ -> return ([],[])

testGameOver :: GameState -> Bool
testGameOver os = (os ^. gsPlayer ^. playerPosition ^. _y) > fromIntegral screenHeight

processParticle :: (Functor m,Monad m,MonadGame m) => GameState -> Particle -> m ([GameObject],[OutgoingAction])
processParticle _ b = do
  currentTicks' <- gcurrentTicks
  anim <- glookupAnimUnsafe (b ^. particleIdentifier)
  if (b ^. particleInception) `plusDuration` (anim ^. animLifetime)  < currentTicks'
    then return ([],[])
    else return ([ObjectParticle b],[])

processPlayerObject :: (MonadGame m,Monad m) => GameState -> [IncomingAction] -> Player -> m (Player,[GameObject],[OutgoingAction])
processPlayerObject gs ias p = case p ^. playerMode of
  Ground -> processGroundPlayerObject gs ias p
  Air -> processAirPlayerObject gs ias p

-- Testet, ob ein LineSegment (ein Sensor) mit der Umgebung kollidiert
lineCollision :: [GameObject] -> LineSegmentReal -> Maybe GameObject
lineCollision boxes l = (getFirst . mconcat . map (First . collides l)) boxes
  where collides :: LineSegmentReal -> GameObject -> Maybe GameObject
        collides l1 bp@(ObjectBox b) = ifMaybe (rectLineSegmentIntersects 0.01 (b ^. boxPosition) l1) bp
        collides _ _ = Nothing


applySensors :: [GameObject] -> PointReal -> Real -> Sensors
applySensors go p wSDev = Sensors wS fSL fSR cSL cSR wSCollision fSLCollision fSRCollision cSLCollision cSRCollision
  where boxes = filter isBox go
        wSCollision = lineCollision boxes wS
        wS = LineSegment (V2 wSL wSY) (V2 wSR wSY)
        wSY = (p ^. _y) + wSDev
        wSL = (p ^. _x) - gcWSSize
        wSR = (p ^. _x) + gcWSSize
        fSLX = (p ^. _x) - 9.0
        fSRX = (p ^. _x) + 9.0
        fSYTop = (p ^. _y) + gcPlayerHeight
        fSYBottom = (p ^. _y) + gcPlayerHeight + 16.0
        cSYTop = (p ^. _y) - gcPlayerHeight
        cSYBottom = (p ^. _y) - gcPlayerHeight - 16.0
        fSL = LineSegment (V2 fSLX fSYTop) (V2 fSLX fSYBottom)
        fSR = LineSegment (V2 fSRX fSYTop) (V2 fSRX fSYBottom)
        cSL = LineSegment (V2 fSLX cSYTop) (V2 fSLX cSYBottom)
        cSR = LineSegment (V2 fSRX cSYTop) (V2 fSRX cSYBottom)
        fSLCollision = lineCollision boxes fSL
        fSRCollision = lineCollision boxes fSR
        cSLCollision = lineCollision boxes cSL
        cSRCollision = lineCollision boxes cSR

processGroundPlayerObject :: (Monad m,MonadGame m) => GameState -> [IncomingAction] -> Player -> m (Player,[GameObject],[OutgoingAction])
processGroundPlayerObject gs ias p = do
  t <- gcurrentTimeDelta
  let   sensorLines = map (ObjectSensorLine . SensorLine) [sensors ^. sensW,sensors ^. sensFL,sensors ^. sensFR,sensors ^. sensCL,sensors ^. sensCR]
        sensors = applySensors (gs ^. gsAllObjects) (p ^. playerPosition) 4.0
        fCollision = (sensors ^. sensFLCollision) <|> (sensors ^. sensFRCollision)
        newPlayerMode = if isNothing fCollision || PlayerJump `elem` ias
                        then Air
                        else Ground
        newPlayerPositionX = case sensors ^. sensWCollision of
          Just (ObjectBox (Box r _ _)) -> if r ^. center . _x < p ^. playerPosition . _x
                                     then r ^. right + gcWSSize + 1.0
                                     else r ^. left - (gcWSSize + 1.0)
          _ -> p ^. playerPosition . _x + toSeconds t * p ^. playerVelocity ^. _x
        newPlayerPositionY = case fCollision of
          Just (ObjectBox (Box r _ _)) -> r ^. top - gcPlayerHeight
          _ -> p ^. playerPosition . _y
        newPlayerVelocityY = if PlayerJump `elem` ias then gcJmp else 0.0
        oldPlayerVelocityX = p ^. playerVelocity ^. _x
        playerAcc
          | PlayerLeft `elem` ias = Just $ if oldPlayerVelocityX > 0.0 then -gcDec else (-gcAcc)
          | PlayerRight `elem` ias = Just $ if oldPlayerVelocityX < 0.0 then gcDec else gcAcc
          | otherwise = Nothing
        newPlayerVelocityX = case playerAcc of
          Just v -> clampAbs gcPlayerMaxSpeed $ oldPlayerVelocityX + v
          Nothing -> oldPlayerVelocityX - min (abs oldPlayerVelocityX) gcFrc * signum oldPlayerVelocityX
        newPlayerVelocity = if isJust (sensors ^. sensWCollision)
                            then V2 0.0 0.0
                            else V2 newPlayerVelocityX newPlayerVelocityY
        np = Player {
          _playerPosition = V2 newPlayerPositionX newPlayerPositionY,
          _playerMode = newPlayerMode,
          _playerVelocity = newPlayerVelocity,
          _playerWalkSince = if newPlayerMode == Ground then p ^. playerWalkSince else Nothing
          }
  return (np,sensorLines,[])

processAirPlayerObject :: (Monad m,MonadGame m) => GameState -> [IncomingAction] -> Player -> m (Player,[GameObject],[OutgoingAction])
processAirPlayerObject gs ias p = do
  t <- gcurrentTimeDelta
  currentTicks' <- gcurrentTicks
  let
    sensorLines = map (ObjectSensorLine . SensorLine) [sensors ^. sensW,sensors ^. sensFL,sensors ^. sensFR,sensors ^. sensCL,sensors ^. sensCR]
    sensors = applySensors (gs ^. gsAllObjects) (p ^. playerPosition) 0.0
    fCollision = (sensors ^. sensFLCollision) <|> (sensors ^. sensFRCollision)
    cCollision = (sensors ^. sensCLCollision) <|> (sensors ^. sensCRCollision)
    movingDownwards = p ^. playerVelocity . _y >= 0.0
    oldPlayerPositionX = p ^. playerPosition . _x
    oldPlayerPositionY = p ^. playerPosition . _y
    oldPlayerVelocityX = p ^. playerVelocity . _x
    oldPlayerVelocityY = p ^. playerVelocity . _y
    playerIsAboveBox b = case b of
      (ObjectBox (Box r _ _)) -> oldPlayerPositionY - gcPlayerHeight < (r ^. bottom)
      _ -> False
    playerIsBelowBox b = case b of
      (ObjectBox (Box r _ _)) -> oldPlayerPositionY + gcPlayerHeight > (r ^. top)
      _ -> False
    -- Ist der naechste Zustand der Bodenzustand?
    newPlayerMode = case fCollision of
      Just b -> if movingDownwards && playerIsBelowBox b
                then Ground
                else Air
      _ -> Air
    dirt =
      if newPlayerMode == Ground
      then [ObjectParticle (Particle "dirt" (p ^. playerPosition) currentTicks')]
      else []
    newPlayerPositionX = case sensors ^. sensWCollision of
      Just (ObjectBox (Box r _ _)) -> if r ^. center . _x < oldPlayerPositionX
                                 then r ^. right  + gcWSSize + 1.0
                                 else r ^. left - (gcWSSize + 1.0)
      _ -> oldPlayerPositionX + toSeconds t * oldPlayerVelocityX
    newPlayerPositionY = case cCollision of
      Just b@(ObjectBox (Box r _ _)) -> if oldPlayerVelocityY < 0.0 && playerIsAboveBox b
                                    then r ^. bottom + gcPlayerHeight
                                    else oldPlayerPositionY + toSeconds t * oldPlayerVelocityY
      _ -> oldPlayerPositionY + toSeconds t * oldPlayerVelocityY
    playerAcc
      | PlayerLeft `elem` ias = Just (-gcAir)
      | PlayerRight `elem` ias = Just gcAir
      | otherwise = Nothing
    newPlayerVelocityX = if isJust (sensors ^. sensWCollision)
                         then 0.0
                         else case playerAcc of
                           Just v -> clampAbs gcPlayerMaxSpeed $ oldPlayerVelocityX + v
                           Nothing -> airDrag oldPlayerVelocityX
    newPlayerVelocityY
      | oldPlayerVelocityY < 0.0 && isJust cCollision && playerIsAboveBox (fromJust cCollision) = toSeconds t * gcGrv
      | oldPlayerVelocityY < (-4.0) && (PlayerJump `onotElem` ias) = -4.0
      | otherwise = oldPlayerVelocityY + toSeconds t * gcGrv
    airDrag xv = if newPlayerVelocityY < 0.0 && newPlayerVelocityY > -4.0 && abs xv >= 0.125
                 then xv * 0.9685
                 else xv
    np = Player {
      _playerPosition = V2 newPlayerPositionX newPlayerPositionY,
      _playerMode = newPlayerMode,
      _playerVelocity = V2 newPlayerVelocityX newPlayerVelocityY,
      _playerWalkSince = if newPlayerMode == Ground then Just currentTicks' else Nothing
      }
  return (np,dirt <> sensorLines,[])
