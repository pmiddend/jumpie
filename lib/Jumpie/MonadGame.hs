{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Jumpie.MonadGame(
    GameData
  , MonadGame(..)
  , runGame
  , glookupAnimUnsafe
  , glookupImageRectangleUnsafe
  ) where

import           Control.Monad.State.Strict (MonadState, StateT, evalStateT,
                                             get, gets, put)
import           Control.Monad.Writer.Lazy (WriterT)
import qualified Data.Set                   as S
import           ClassyPrelude
import Data.Maybe(fromJust)
import           Jumpie.GameConfig
import           Jumpie.Types               (Keydowns)
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
import System.Random(StdGen,getStdGen)
import Control.Monad.Random(MonadRandom(..),RandT,evalRandT)

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

newtype GameDataM p a = GameDataM {
  runGameData :: RandT StdGen (StateT (GameData p) IO) a
  } deriving(Monad,MonadRandom,MonadIO,MonadState (GameData p),Applicative,Functor)

instance (Monad m,MonadGame m) => MonadGame (StateT n m) where
  gpollEvents = lift gpollEvents
  gupdateTicks = lift gupdateTicks
  gupdateKeydowns e = lift (gupdateKeydowns e)
  gcurrentTicks = lift gcurrentTicks
  gcurrentTimeDelta = lift gcurrentTimeDelta
  gcurrentKeydowns = lift gcurrentKeydowns
  grender p = lift (grender p)
  glookupAnim aid = lift (glookupAnim aid)
  glookupImageRectangle i = lift (glookupImageRectangle i)

instance (Monad m,MonadGame m,Monoid w) => MonadGame (WriterT w m) where
  gpollEvents = lift gpollEvents
  gupdateTicks = lift gupdateTicks
  gupdateKeydowns e = lift (gupdateKeydowns e)
  gcurrentTicks = lift gcurrentTicks
  gcurrentTimeDelta = lift gcurrentTimeDelta
  gcurrentKeydowns = lift gcurrentKeydowns
  grender p = lift (grender p)
  glookupAnim aid = lift (glookupAnim aid)
  glookupImageRectangle i = lift (glookupImageRectangle i)

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

processKeydown :: Event -> Keydowns -> Keydowns
processKeydown (Keyboard KeyUp _ keysym) = S.delete keysym
processKeydown (Keyboard KeyDown _ keysym) = S.insert keysym
processKeydown _ = id

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns k es = foldr processKeydown k es

runGame :: P.WindowTitle -> P.WindowSize -> GameDataM PlatformBackend () -> IO ()
runGame title size action = withPlatform title size $
  \platform -> do
    (images, anims) <- readMediaFiles (P.loadImage platform) mediaDir
    ticks <- getTicks
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
    r <- getStdGen
    evalStateT (evalRandT (runGameData action) r) gameData
