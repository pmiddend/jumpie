module Jumpie.Render(
    render
  , renderAll
  , renderClear
  , renderFinish
) where

import           Numeric.Lens              (dividing)
--import           Debug.Trace                 (trace)
import           Control.Monad.State.Strict (gets)
import           Jumpie.Commandize          (RenderCommand (..),
                                             RenderPositionMode (..))
import           Jumpie.GameConfig          (screenHeight, screenWidth)
import           Jumpie.GameData            (GameData, GameDataM,
                                             gdSurfaces,renderFinish,renderClear,renderSprites)
import           Jumpie.Geometry.Point      (Point2 (Point2))
import           Jumpie.Geometry.Rect       (Rect (Rect), dimensions)
import           Jumpie.Types               (PointInt)
import           Prelude                    (Double, Fractional, Integral, abs,
                                             div, error, error, floor,
                                             fromIntegral, mod, undefined, (*),
                                             (+), (-), (/))
import           Linear.Matrix             (M33, (!*), (!*!))
import           Linear.V2                 (V2 (..), _x, _y)
import           Linear.V3                 (V3 (..))
import ClassyPrelude
import Wrench.Color
import Wrench.ImageData(findSurfaceUnsafe)
import           Control.Lens              ((&), (^.))
import           Control.Lens.Getter       (Getter, to)
import           Control.Lens.Setter       ((%~), (+~), (.~))
import           Control.Lens.TH           (makeLenses)
import Wrench.Rectangle
import Wrench.Platform(SpriteInstance(..),Platform)

{-
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
-}

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

renderAll :: Platform p => [RenderCommand] -> GameDataM p ()
renderAll gd = mapM_ render gd

toV2 :: Getter (V3 a) (V2 a)
toV2 = to toV2'
  where toV2' (V3 x y _) = V2 x y

render :: Platform p => RenderCommand -> GameDataM p ()
render ob = case ob of
  FillScreen color -> renderClear color
  RenderLine _ _ -> return ()
  RenderBackground identifier -> do
    surfaces <- gets gdSurfaces
    let (image,srcRect) = findSurfaceUnsafe surfaces identifier
        destRect = rectangleFromPoints (V2 0 0) (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))
        rot = 0
    renderSprites [SpriteInstance image srcRect destRect rot]
  --RenderLine color lineSegment -> surfaceBresenham (gdScreen gd) color lineSegment
  RenderSprite identifier pos' mode -> do
    surfaces <- gets gdSurfaces
    let (image,srcRect') = findSurfaceUnsafe surfaces identifier
        srcRectDim = srcRect' ^. rectangleDimensions
        pos = case mode of
            RenderPositionCenter -> (fromIntegral <$> pos') - ((/2) <$> srcRectDim)
            RenderPositionTopLeft -> fromIntegral <$> pos'
        rot = 0
        destRect = rectangleFromPoints pos (pos + srcRectDim)
    renderSprites [SpriteInstance image srcRect' destRect rot]

