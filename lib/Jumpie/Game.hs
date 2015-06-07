{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Jumpie.Game(processGameObjects,testGameOver) where

import           Data.Maybe                   (fromJust)
import           Data.Monoid                  (First (First), getFirst)
import Wrench.Time
import           Jumpie.GameConfig
import Control.Lens.Fold(maximumOf,minimumOf)
import           Jumpie.MonadGame
import           Jumpie.GameObject
import           Jumpie.Platform
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
import Control.Lens((^.),(^?!),_Just,use)
import Wrench.Animation
import Data.List(last)
import Control.Lens((.=),(<>=),(%=))
import Control.Monad.Random(MonadRandom)
import Control.Monad.State.Strict(MonadState)
import Control.Monad.Writer(MonadWriter)

updateCameraPosition :: PointReal -> PointReal -> PointReal
updateCameraPosition cameraPos playerPos =
  let
    k = cameraPos ^. _x
    px = playerPos ^. _x
    w = fromIntegral screenWidth
    z = fromIntegral cameraTolerance
    x = min (max k ((w-z)/2 + px - w)) (px - (w-z)/2)
  in
    V2 x (cameraPos ^. _y)

type LastWorldSection = WorldSection

shouldGenerateNewSection :: LastWorldSection -> Player -> Bool
shouldGenerateNewSection lastSection player =
  let maxBoxPosition = minimumOf (traverse . _ObjectPlatform . platLeftAbsReal) lastSection ^?! _Just
  in (abs ((player ^. playerPosition . _x) - maxBoxPosition)) < fromIntegral screenWidth

generateNewSection :: (MonadRandom m,MonadGame m,Monad m,MonadIO m,Applicative m,MonadState GameState m) => WorldSection -> m ()
generateNewSection lastSection = do
  maxDeadlinePrev <- use gsMaxDeadline
  let lastSectionEnd = maximumOf (traverse . _ObjectPlatform . platRight) lastSection ^?! _Just
  newSection <- moveSection (TileIncrement (lastSectionEnd+1)) <$> generateSection maxDeadlinePrev
  gsMaxDeadline .= maximumOf (traverse . _ObjectPlatform . platDeadline) newSection ^?! _Just
  gsSections <>= [newSection]

updateSectionsAndPlayer :: (MonadIO m,MonadGame m, MonadWriter [OutgoingAction] m, MonadState GameState m, Applicative m) => [IncomingAction] -> m ()
updateSectionsAndPlayer actions = do
  newSections <- traverse (processWorldSection actions) =<< use gsSections 
  gsSections .= newSections
  newTempSection <- processWorldSection actions =<< use gsTempSection 
  gsTempSection .= newTempSection
  (newPlayer,playerObjects) <- processPlayerObject actions =<< use gsPlayer
  gsTempSection <>= playerObjects
  gsPlayer .= newPlayer

maybeGenerateNewSection :: (MonadRandom m,Monad m,MonadIO m,Applicative m,MonadGame m,MonadState GameState m,MonadWriter [OutgoingAction] m) => m ()
maybeGenerateNewSection = do
  sections <- use gsSections
  player <- use gsPlayer
  when (shouldGenerateNewSection (last sections) player) (generateNewSection (last sections))

processGameObjects :: (MonadRandom m,Monad m,MonadIO m,Applicative m,MonadGame m,MonadState GameState m,MonadWriter [OutgoingAction] m) => [IncomingAction] -> m ()
processGameObjects actions = do
  updateSectionsAndPlayer actions
  maybeGenerateNewSection
  sections <- use gsSections
  case sections of
    [] -> error "Empty list of sections"
    []:ss@(newFirstSection:_) -> do
      putStrLn "Moving sections"
      let (newFirstSectionStart,_) = sectionBeginEnd newFirstSection
      tempSection <- use gsTempSection
      -- TODO: camera position only changes x value - better lens here
      gsPlayer %= (`moveObject` (-newFirstSectionStart))
      --gsPlayer . playerPosition . _x -= newFirstSectionStart
      player <- use gsPlayer
      cameraPosition <- use gsCameraPosition
      gsCameraPosition .= updateCameraPosition (V2 (cameraPosition ^. _x - (tileIncrementAbsReal newFirstSectionStart)) (cameraPosition ^. _y)) (player ^. playerPosition) 
      gsTempSection .= moveSection (-newFirstSectionStart) tempSection
      gsSections .= (moveSection (-newFirstSectionStart) <$> ss)
    _ -> do
      player <- use gsPlayer
      cameraPosition <- use gsCameraPosition
      gsCameraPosition .= updateCameraPosition cameraPosition (player ^. playerPosition) 
  

processWorldSection :: (Monad m,MonadIO m,Applicative m,MonadGame m,MonadState GameState m,MonadWriter [OutgoingAction] m) => [IncomingAction] -> WorldSection -> m WorldSection
processWorldSection actions section = do
  objects <- traverse (processGameObject actions) section
  return (concat objects)

processGameObject :: (MonadIO m,Functor m,MonadGame m,Monad m,MonadState GameState m,MonadWriter [OutgoingAction] m) => [IncomingAction] -> GameObject -> m [GameObject]
processGameObject _ o = case o of
  ObjectPlayer _ -> error "player given to processGameObject, check the code"
  ObjectPlatform b -> do
    ticks <- gcurrentTicks
    if ticks > b ^. platDeadline
      then return []
      else return [ObjectPlatform b]
  ObjectParticle b -> processParticle b
  ObjectSensorLine _ -> return []

testGameOver :: MonadState GameState m => m Bool
testGameOver = do
  player <- use gsPlayer
  return ((player ^. playerPosition ^. _y) > fromIntegral screenHeight)

processParticle :: (Functor m,Monad m,MonadGame m,MonadState GameState m,MonadWriter [OutgoingAction] m) => Particle -> m [GameObject]
processParticle b = do
  currentTicks' <- gcurrentTicks
  anim <- glookupAnimUnsafe (b ^. particleIdentifier)
  if (b ^. particleInception) `plusDuration` (anim ^. animLifetime)  < currentTicks'
    then return []
    else return [ObjectParticle b]

processPlayerObject :: (MonadGame m,Monad m,MonadState GameState m,MonadWriter [OutgoingAction] m) => [IncomingAction] -> Player -> m (Player,[GameObject])
processPlayerObject ias p = case p ^. playerMode of
  Ground -> processGroundPlayerObject ias p
  Air -> processAirPlayerObject ias p

-- Testet, ob ein LineSegment (ein Sensor) mit der Umgebung kollidiert
lineCollision :: [GameObject] -> LineSegmentReal -> Maybe GameObject
lineCollision boxes l = (getFirst . mconcat . map (First . collides l)) boxes
  where collides :: LineSegmentReal -> GameObject -> Maybe GameObject
        collides l1 bp@(ObjectPlatform b) = ifMaybe (rectLineSegmentIntersects 0.01 (b ^. platRectAbsReal) l1) bp
        collides _ _ = Nothing


applySensors :: [GameObject] -> PointReal -> Real -> Sensors
applySensors go p wSDev = Sensors wS fSL fSR cSL cSR wSCollision fSLCollision fSRCollision cSLCollision cSRCollision
  where boxes = filter isPlatform go
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

processGroundPlayerObject :: (Monad m,MonadGame m,MonadState GameState m,MonadWriter [OutgoingAction] m) => [IncomingAction] -> Player -> m (Player,[GameObject])
processGroundPlayerObject ias p = do
  t <- gcurrentTimeDelta
  objects <- use gsAllObjects
  let   sensorLines = map (ObjectSensorLine . SensorLine) [sensors ^. sensW,sensors ^. sensFL,sensors ^. sensFR,sensors ^. sensCL,sensors ^. sensCR]
        sensors = applySensors objects (p ^. playerPosition) 4.0
        fCollision = (sensors ^. sensFLCollision) <|> (sensors ^. sensFRCollision)
        newPlayerMode = if isNothing fCollision || PlayerJump `elem` ias
                        then Air
                        else Ground
        newPlayerPositionX = case sensors ^. sensWCollision of
          Just (ObjectPlatform r) -> if r ^. platRectAbsReal . center . _x < p ^. playerPosition . _x
                                     then r ^. platRectAbsReal . right + gcWSSize + 1.0
                                     else r ^. platRectAbsReal . left - (gcWSSize + 1.0)
          _ -> p ^. playerPosition . _x + toSeconds t * p ^. playerVelocity ^. _x
        newPlayerPositionY = case fCollision of
          Just (ObjectPlatform r) -> r ^. platRectAbsReal . top - gcPlayerHeight
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
  return (np,sensorLines)

processAirPlayerObject :: (Monad m,MonadGame m,MonadState GameState m,MonadWriter [OutgoingAction] m) => [IncomingAction] -> Player -> m (Player,[GameObject])
processAirPlayerObject ias p = do
  t <- gcurrentTimeDelta
  objects <- use gsAllObjects
  currentTicks' <- gcurrentTicks
  let
    sensorLines = map (ObjectSensorLine . SensorLine) [sensors ^. sensW,sensors ^. sensFL,sensors ^. sensFR,sensors ^. sensCL,sensors ^. sensCR]
    sensors = applySensors objects (p ^. playerPosition) 0.0
    fCollision = (sensors ^. sensFLCollision) <|> (sensors ^. sensFRCollision)
    cCollision = (sensors ^. sensCLCollision) <|> (sensors ^. sensCRCollision)
    movingDownwards = p ^. playerVelocity . _y >= 0.0
    oldPlayerPositionX = p ^. playerPosition . _x
    oldPlayerPositionY = p ^. playerPosition . _y
    oldPlayerVelocityX = p ^. playerVelocity . _x
    oldPlayerVelocityY = p ^. playerVelocity . _y
    playerIsAboveBox b = case b of
      (ObjectPlatform r) -> oldPlayerPositionY - gcPlayerHeight < (r ^. platRectAbsReal . bottom)
      _ -> False
    playerIsBelowBox b = case b of
      (ObjectPlatform r) -> oldPlayerPositionY + gcPlayerHeight > (r ^. platRectAbsReal . top)
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
      Just (ObjectPlatform r) -> if r ^. platRectAbsReal . center . _x < oldPlayerPositionX
                                 then r ^. platRectAbsReal . right  + gcWSSize + 1.0
                                 else r ^. platRectAbsReal . left - (gcWSSize + 1.0)
      _ -> oldPlayerPositionX + toSeconds t * oldPlayerVelocityX
    newPlayerPositionY = case cCollision of
      Just b@(ObjectPlatform r) -> if oldPlayerVelocityY < 0.0 && playerIsAboveBox b
                                    then r ^. platRectAbsReal . bottom + gcPlayerHeight
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
  return (np,dirt <> sensorLines)
