{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jumpie.GameData(
    GameData(..)
  , updateKeydowns
  , updateTicks
  , GameDataM
  , runGame
  , pollEvents
  , renderBegin
  , renderFinish
  , renderSprites
  , renderClear
  , render
  , lookupAnimSafe
  , lookupSurfaceSafe
  ) where

import qualified Data.Set as S
import           Control.Monad.State.Strict (StateT, get, gets, put, runStateT,MonadState)


import           Jumpie.GameConfig          (gcTimeMultiplier)

import           Control.Monad.Random       (RandT, evalRandT,MonadRandom)
import           System.Random              (StdGen)
import qualified Wrench.Platform as P
import Wrench.Time
import Wrench.Picture
import Wrench.Engine
import Wrench.ImageData
import Wrench.Color
import Wrench.KeyMovement
import Wrench.Platform(Platform)
import Wrench.Animation
import Wrench.AnimId
import           Jumpie.Types               (Keydowns)
import ClassyPrelude
import           Data.Map.Strict             ((!))

data GameData p = GameData {
    gdSurfaces     :: SurfaceMap (P.PlatformImage p)
  , gdAnims        :: AnimMap
  , gdPlatform     :: p
  , gdCurrentTicks :: !TimeTicks
  , gdTimeDelta    :: !TimeDelta
  , gdKeydowns     :: !Keydowns
  , gdFont         :: P.PlatformFont p
  }

lookupAnimSafe :: GameData p -> AnimId -> Animation
lookupAnimSafe gd aid = gdAnims gd ! aid

lookupSurfaceSafe :: Platform p => GameData p -> ImageId -> SurfaceData (P.PlatformImage p)
lookupSurfaceSafe gd sid = gdSurfaces gd ! sid

type GameDataBaseM p = StateT (GameData p) IO

newtype GameDataM p a = GameDataM {
  runGameData :: RandT StdGen (GameDataBaseM p) a
  } deriving(Monad,MonadRandom,MonadIO,MonadState (GameData p),Applicative,Functor)

updateTicks :: GameDataM p ()
updateTicks = do
  oldTicks <- gets gdCurrentTicks
  newTicks <- liftIO getTicks
  s <- get
  put s {
    gdCurrentTicks = newTicks,
    gdTimeDelta = (fromSeconds gcTimeMultiplier) * (newTicks `tickDelta` oldTicks)
    }

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns k es = (k \\ keyUps) `union` keyDowns
  where keyUps = S.fromList ((map toKey  . filter isKeyUp) es)
        keyDowns = S.fromList ((map toKey  . filter isKeyDown) es)
        isKeyUp (Keyboard KeyUp _ _) = True
        isKeyUp _ = False
        isKeyDown (Keyboard KeyDown _ _) = True
        isKeyDown _ = False
        toKey e = case e of
          Keyboard _ _ keysym -> keysym
          _ -> undefined

updateKeydowns :: [Event] -> GameDataM p ()
updateKeydowns events = do
  oldKeydowns <- gets gdKeydowns
  s <- get
  put s { gdKeydowns = processKeydowns oldKeydowns events }

pollEvents :: P.Platform p => GameDataM p [Event]
pollEvents = do
  p <- gets gdPlatform
  liftIO $ P.pollEvents p

renderBegin :: P.Platform p => GameDataM p ()
renderBegin = do
  p <- gets gdPlatform
  liftIO $ P.renderBegin p

render :: P.Platform p => Picture -> GameDataM p ()
render picture = do
  p <- gets gdPlatform
  sf <- gets gdSurfaces
  font <- gets gdFont
  liftIO $ wrenchRender p sf font Nothing picture

renderClear :: P.Platform p => Color -> GameDataM p ()
renderClear color = do
  p <- gets gdPlatform
  liftIO $ P.renderClear p color

renderFinish :: P.Platform p => GameDataM p ()
renderFinish = do
  p <- gets gdPlatform
  liftIO $ P.renderFinish p

renderSprites :: P.Platform p => [P.SpriteInstance (P.PlatformImage p)] -> GameDataM p ()
renderSprites ss = do
  p <- gets gdPlatform
  liftIO $ P.renderSprites p ss

runGame :: StdGen -> GameData p -> GameDataM p a -> IO (a,GameData p)
runGame r gameData game = runStateT (evalRandT (runGameData game) r) gameData