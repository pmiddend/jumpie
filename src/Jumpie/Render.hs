module Jumpie.Render(
    render
  , renderAll
  , setRenderDrawColor
  , renderClear
  , renderFinish
) where

import           Control.Applicative        ((<$>))
import           Control.Monad              (mapM_, return, (>>))
import           Control.Monad.IO.Class     (liftIO)
import           Data.Function              (($))
import           Data.Map.Strict            ((!))
import           Data.Tuple                 (snd)
import           Data.Word                  (Word8)
--import           Debug.Trace                 (trace)
import           Control.Monad.State.Strict (gets)
import           Jumpie.Commandize          (RenderCommand (..),
                                             RenderPositionMode (..))
import           Jumpie.GameConfig          (screenHeight, screenWidth)
import           Jumpie.GameData            (GameData, GameDataM, gdRenderer,
                                             gdSurfaces)
import           Jumpie.Geometry.Point      (Point2 (Point2))
import           Jumpie.Geometry.Rect       (Rect (Rect), dimensions)
import           Jumpie.ImageData           (ImageId)
import           Jumpie.Types               (PointInt)
import           Prelude                    (Double, Fractional, Integral, abs,
                                             div, error, error, floor,
                                             fromIntegral, mod, undefined, (*),
                                             (+), (-), (/))


setRenderDrawColor :: Color -> GameDataM ()
setRenderDrawColor c = return (){-do
  platform <- gets gdPlatform
  _ <- liftIO $ SDLV.setRenderDrawColor renderer r g b a
  return ()-}

renderClear :: Color -> GameDataM ()
renderClear color = do
  platform <- gets gdPlatform
  _ <- liftIO $ renderClear color
  return ()

renderFinish :: GameDataM ()
renderFinish = do
  platform <- gets gdPlatform
  liftIO $ renderFinish platform

{-
blitAt :: ImageId -> PointInt -> RenderPositionMode -> GameDataM ()
blitAt image pos mode = do
  renderer <- gets gdRenderer
  surfaces <- gets gdSurfaces
  let realPos = case mode of
          RenderPositionCenter -> pos - ((`div` 2) <$> (dimensions $ snd $ imageData))
          RenderPositionTopLeft -> pos
      imageData = surfaces ! image
  liftIO $ SDLH.blitSameSize imageData realPos renderer
-}

{-
blitBackground :: ImageId -> GameDataM ()
blitBackground image = do
  renderer <- gets gdRenderer
  surfaces <- gets gdSurfaces
  let pos = Point2 0 0
      imageData = surfaces ! image
  liftIO $ SDLH.blitRescale imageData (Rect pos (Point2 screenWidth screenHeight)) renderer
-}

renderAll :: [RenderCommand] -> GameDataM ()
renderAll gd = mapM_ render gd

render :: RenderCommand -> GameDataM ()
render ob = case ob of
  FillScreen color -> renderClear color
  RenderLine _ _ -> return ()
  RenderBackground identifier -> blitBackground identifier
  --RenderLine color lineSegment -> surfaceBresenham (gdScreen gd) color lineSegment
  RenderSprite identifier pos mode -> blitAt identifier pos mode
