{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module Jumpie.MonadGame(
    GameData
  , MonadGame(..)
  , runGame
  , glookupAnimUnsafe
  , glookupImageRectangleUnsafe
  ) where

import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             get, gets, put)
import qualified Data.Set                   as S


import           ClassyPrelude
import           Control.Monad.Random       (MonadRandom, RandT, evalRandT)
import Data.Maybe(fromJust)
import           Jumpie.GameConfig
import           Jumpie.Types               (Keydowns)
import           System.Random              (StdGen, getStdGen)
import           Wrench.Animation
import           Wrench.AnimId
import           Wrench.Engine
import           Wrench.ImageData
import           Wrench.KeyMovement
import           Wrench.Picture
import           Wrench.Platform            (Platform)
import qualified Wrench.Platform            as P
import           Wrench.Rectangle
import           Wrench.Time

class MonadGame m where
  gpollEvents :: m [Event]
  gupdateTicks :: m ()
  gupdateKeydowns :: [Event] -> m ()
  gcurrentTicks :: m TimeTicks
  gcurrentTimeDelta :: m TimeDelta
  gcurrentKeydowns :: m Keydowns
  grender :: Picture -> m ()
  glookupAnim :: AnimId -> m (Maybe Animation)
  glookupImageRectangle :: ImageId -> m (Maybe Rectangle)

glookupAnimUnsafe :: (Functor m,MonadGame m) => AnimId -> m Animation 
glookupAnimUnsafe anim = fromJust <$> (glookupAnim anim)

glookupImageRectangleUnsafe :: (Functor m,MonadGame m) => ImageId -> m Rectangle
glookupImageRectangleUnsafe im = fromJust <$> (glookupImageRectangle im)

data GameData p = GameData {
    gdSurfaces     :: SurfaceMap (P.PlatformImage p)
  , gdAnims        :: AnimMap
  , gdPlatform     :: p
  , gdCurrentTicks :: !TimeTicks
  , gdTimeDelta    :: !TimeDelta
  , gdKeydowns     :: !Keydowns
  , gdFont         :: P.PlatformFont p
  }

type GameDataBaseM p = StateT (GameData p) IO

newtype GameDataM p a = GameDataM {
  runGameData :: RandT StdGen (GameDataBaseM p) a
  } deriving(Monad,MonadRandom,MonadIO,MonadState (GameData p),Applicative,Functor)

instance Platform p => MonadGame (GameDataM p) where
  gpollEvents = do
    p <- gets gdPlatform
    liftIO $ P.pollEvents p
  gupdateTicks = do
    oldTicks <- gets gdCurrentTicks
    newTicks <- liftIO getTicks
    s <- get
    put s {
        gdCurrentTicks = newTicks,
        gdTimeDelta = (fromSeconds gcTimeMultiplier) * (newTicks `tickDelta` oldTicks)
        }
  gupdateKeydowns events = do
    oldKeydowns <- gets gdKeydowns
    s <- get
    put s { gdKeydowns = processKeydowns oldKeydowns events }
  gcurrentTicks = gets gdCurrentTicks
  gcurrentTimeDelta = gets gdTimeDelta
  gcurrentKeydowns = gets gdKeydowns
  grender picture = do
    p <- gets gdPlatform
    sf <- gets gdSurfaces
    font <- gets gdFont
    liftIO $ wrenchRender p sf font Nothing picture
  glookupAnim aid = do
    anims <- gets gdAnims
    return (aid `lookup` anims)
  glookupImageRectangle sid = do
    surfaces <- gets gdSurfaces
    return (snd <$> (sid `lookup` surfaces))

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns k es = (k \\ keyUps) `union` keyDowns
  where keyUps = S.fromList (es >>= keyUpSym)
        keyDowns = S.fromList (es >>= keyDownSym)
        keyUpSym (Keyboard KeyUp _ keysym) = [keysym]
        keyUpSym _ = []
        keyDownSym (Keyboard KeyDown _ keysym) = [keysym]
        keyDownSym _ = []

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
