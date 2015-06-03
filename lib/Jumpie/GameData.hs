{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module Jumpie.GameData(
    GameData
  , Game(..)
  , runGame
  ) where

import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             get, gets, put)
import qualified Data.Set                   as S


import           ClassyPrelude
import           Control.Monad.Random       (MonadRandom, RandT, evalRandT)
import           Data.Map.Strict            ((!))
import           Jumpie.GameConfig
import           Jumpie.Types               (Keydowns)
import           System.Random              (StdGen, getStdGen)
import           Wrench.Animation
import           Wrench.AnimId
import           Wrench.Color
import           Wrench.Engine
import           Wrench.ImageData
import           Wrench.KeyMovement
import           Wrench.Picture
import           Wrench.Platform            (Platform)
import qualified Wrench.Platform            as P
import           Wrench.Rectangle
import           Wrench.Time

class Game m where
  gpollEvents :: m [Event]
  gupdateTicks :: m ()
  gupdateKeydowns :: [Event] -> m ()
  gcurrentTicks :: m TimeTicks
  gcurrentTimeDelta :: m TimeDelta
  gcurrentKeydowns :: m Keydowns
  grender :: Picture -> m ()
  glookupAnim :: AnimId -> m Animation
  glookupImageRectangle :: ImageId -> m Rectangle

data GameData p = GameData {
    gdSurfaces     :: SurfaceMap (P.PlatformImage p)
  , gdAnims        :: AnimMap
  , gdPlatform     :: p
  , gdCurrentTicks :: !TimeTicks
  , gdTimeDelta    :: !TimeDelta
  , gdKeydowns     :: !Keydowns
  , gdFont         :: P.PlatformFont p
  }

lookupAnimSafe :: AnimId -> GameDataM p Animation
lookupAnimSafe aid = do
  anims <- gets gdAnims
  return (anims ! aid)

lookupSurfaceSafe :: Platform p => ImageId -> GameDataM p (SurfaceData (P.PlatformImage p))
lookupSurfaceSafe sid = do
  surfaces <- gets gdSurfaces
  return (surfaces ! sid)

lookupSurfaceRectangleSafe :: Platform p => ImageId -> GameDataM p Rectangle
lookupSurfaceRectangleSafe sid = do
  surfaces <- gets gdSurfaces
  return (snd (surfaces ! sid))

type GameDataBaseM p = StateT (GameData p) IO

newtype GameDataM p a = GameDataM {
  runGameData :: RandT StdGen (GameDataBaseM p) a
  } deriving(Monad,MonadRandom,MonadIO,MonadState (GameData p),Applicative,Functor)

instance Platform p => Game (GameDataM p) where
  gpollEvents = pollEvents
  gupdateTicks = updateTicks
  gupdateKeydowns = updateKeydowns
  gcurrentTicks = currentTicks
  gcurrentTimeDelta = currentTimeDelta
  gcurrentKeydowns = currentKeydowns
  grender = render
  glookupAnim = lookupAnimSafe
  glookupImageRectangle = lookupSurfaceRectangleSafe

currentTicks :: GameDataM p TimeTicks
currentTicks = gets gdCurrentTicks

currentKeydowns :: GameDataM p Keydowns
currentKeydowns = gets gdKeydowns

currentTimeDelta :: GameDataM p TimeDelta
currentTimeDelta = gets gdTimeDelta

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

runGame' :: StdGen -> GameData p -> GameDataM p a -> IO a
runGame' r gameData game = evalStateT (evalRandT (runGameData game) r) gameData

runGame :: P.WindowTitle -> P.WindowSize -> GameDataM PlatformBackend () -> IO ()
runGame title size action = withPlatform title size $
  \platform -> do
    (images, anims) <- readMediaFiles (P.loadImage platform) mediaDir
    ticks <- getTicks
    g <- getStdGen
    font <- P.loadFont platform (mediaDir <> "/stdfont.ttf") 15
    let
      gameData = GameData {
          gdSurfaces = images
        , gdAnims = anims
        , gdPlatform = platform
        , gdCurrentTicks = ticks
        , gdTimeDelta = fromSeconds 0
        , gdKeydowns = mempty
        , gdFont = font
        }
    runGame' g gameData action
