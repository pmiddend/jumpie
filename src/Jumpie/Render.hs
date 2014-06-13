module Jumpie.Render(
  render,
  renderAll,
  setRenderDrawColor,
  renderClear,
  renderFinish) where

import           Control.Applicative        ((<$>))
import           Control.Monad              (mapM_, return, (>>))
import           Control.Monad.IO.Class     (liftIO)
import           Data.Function              (($))
import           Data.Map.Strict            ((!))
import           Data.Tuple                 (snd)
import           Data.Word                  (Word8)
--import           Debug.Trace                 (trace)
import           Control.Monad.State.Strict (get)
import qualified Graphics.UI.SDL.Video      as SDLV
import           Jumpie.Commandize          (RenderCommand (..),
                                             RenderPositionMode (..))
import           Jumpie.GameData            (GameData, GameDataM, gdRenderer,
                                             gdSurfaces)
import           Jumpie.Geometry.Rect       (dimensions)
import           Jumpie.ImageData           (ImageId)
import qualified Jumpie.SDLHelper           as SDLH
import           Jumpie.Types               (PointInt)
import           Prelude                    (Double, Fractional, Integral, abs,
                                             div, error, error, floor,
                                             fromIntegral, mod, undefined, (*),
                                             (+), (-), (/))


setRenderDrawColor :: Word8 -> Word8 -> Word8 -> Word8 -> GameDataM ()
setRenderDrawColor r g b a = do
  renderer <- gdRenderer <$> get
  liftIO $ SDLV.setRenderDrawColor renderer r g b a
  return ()

renderClear :: GameDataM ()
renderClear = do
  renderer <- gdRenderer <$> get
  liftIO $ SDLV.renderClear renderer
  return ()

renderFinish :: GameDataM ()
renderFinish = do
  renderer <- gdRenderer <$> get
  liftIO $ SDLH.renderFinish renderer

blitAt :: ImageId -> PointInt -> RenderPositionMode -> GameDataM ()
blitAt image pos mode = do
  renderer <- gdRenderer <$> get
  surfaces <- gdSurfaces <$> get
  let realPos = case mode of
          RenderPositionCenter -> pos - ((`div` 2) <$> (dimensions $ snd $ imageData))
          RenderPositionTopLeft -> pos
      imageData = surfaces ! image
  liftIO $ SDLH.blitAtPosition imageData realPos renderer

renderAll :: [RenderCommand] -> GameDataM ()
renderAll gd = mapM_ render gd

render :: RenderCommand -> GameDataM ()
render ob = case ob of
  FillScreen (r,g,b) -> setRenderDrawColor r g b 255 >> renderClear
  RenderLine _ _ -> return ()
  --RenderLine color lineSegment -> surfaceBresenham (gdScreen gd) color lineSegment
  RenderSprite identifier pos mode -> blitAt identifier pos mode
