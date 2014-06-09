module Jumpie.SDLHelper(
  {-
  putPixel32,
  createPixel,
  surfaceBresenham,
  fillSurface,
-}
  blitAtPosition,
  pollEvents,
  fromSDLRect,
  withWindow,
  withRenderer
  ) where

import           Control.Monad         (return)
import           Data.Function         (($))
import           Data.Int              (Int)
import           Data.Tuple            (fst)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Marshal.Utils (with)
import           Graphics.UI.SDL.Enum  (eventActionGetEvent,
                                        eventTypeFirstEvent, eventTypeLastEvent)
import           Graphics.UI.SDL.Event (peepEvents, pumpEvents)
import qualified Graphics.UI.SDL.Types as SDLT
import           Graphics.UI.SDL.Video (renderCopy)
--import           Jumpie.Bresenham             (bresenham)
--import           Jumpie.Debug                 (traceShowId)
--import           Jumpie.Geometry.Intersection (lineSegmentInsideRect)
--import           Jumpie.Geometry.LineSegment  (LineSegment)
import           Jumpie.Geometry.Point (Point2 (Point2))
import           Jumpie.Geometry.Rect  (Rect (Rect), dimensions)
--import           Jumpie.Geometry.Rect         (inside)
import           Jumpie.ImageData      (SurfaceData)
--import           Jumpie.Monad                 (when_)
import           Control.Exception     (bracket)
import           Data.String           (String)
import           Foreign.C.String      (withCStringLen)
import           Graphics.UI.SDL.Enum  (windowPosUndefined, windowPosUndefined)
import           Graphics.UI.SDL.Types (Renderer, Window)
import           Graphics.UI.SDL.Video (createRenderer, createWindow,
                                        destroyRenderer, destroyWindow,
                                        renderPresent, renderSetLogicalSize)
import           Jumpie.Types          (PointInt)
import           Prelude               (RealFrac, floor, fromEnum, fromIntegral,
                                        (*), (+), (-))
import           System.IO             (IO)

screenAbsoluteWidth,screenAbsoluteHeight :: Int
screenAbsoluteWidth = 0
screenAbsoluteHeight = 0
windowFlags :: Int
windowFlags = 0

withWindow :: String -> (Window -> IO a) -> IO a
withWindow title callback = withCStringLen title $ \windowTitle ->
  let acquireResource = createWindow (fst windowTitle) windowPosUndefined windowPosUndefined (fromIntegral screenAbsoluteWidth) (fromIntegral screenAbsoluteHeight) (fromIntegral windowFlags)
      releaseResource = destroyWindow
  in bracket acquireResource releaseResource callback

withRenderer :: Window -> Int -> Int -> (Renderer -> IO a) -> IO a
withRenderer window screenWidth screenHeight callback =
  let acquireResource = createRenderer window (-1) 0
      releaseResource = destroyRenderer
  in bracket acquireResource releaseResource $ \renderer -> do
    renderSetLogicalSize renderer (fromIntegral screenWidth) (fromIntegral screenHeight)
    callback renderer
{-
createPixel :: Surface -> (Word8,Word8,Word8) -> IO Pixel
createPixel s (r,g,b) = mapRGB (surfaceGetPixelFormat s) r g b

putPixel32 :: Point2 Int -> Pixel -> Surface -> IO ()
putPixel32 p (Pixel pixel) s = do
  pixels <- castPtr <$> surfaceGetPixels s
  pokeElemOff pixels (((pY p) * surfaceGetWidth s) + (pX p)) pixel

surfaceBresenham :: Surface -> (Word8,Word8,Word8) -> LineSegment (Point2 Int) -> IO ()
surfaceBresenham s rawColor line = do
  clipRect <- fromSDLRect <$> getClipRect s
  when (lineSegmentInsideRect line clipRect) surfaceBresenham'
  where surfaceBresenham' = do
          _ <- lockSurface s
          color <- createPixel s rawColor
          let points = bresenham line
          forM_ points $ \p -> do
            putPixel32 p color s
          _ <- unlockSurface s
          return ()

fillSurface :: Surface -> (Word8,Word8,Word8) -> IO ()
fillSurface screen color = do
  cr <- getClipRect screen
  pixel <- createPixel screen color
  _ <- fillRect screen (Just cr) pixel
  return ()
-}
renderSprite r t srcrect dstrect = with srcrect $ \srcrectptr -> with dstrect $ \dstrectptr -> renderCopy r t srcrectptr dstrectptr

blitAtPosition :: SurfaceData -> PointInt -> SDLT.Renderer -> IO ()
blitAtPosition (srcSurface,srcRect) pos renderer = do
  let destRect = Rect pos (pos + (dimensions srcRect))
  renderSprite renderer srcSurface (toSDLRect srcRect) (toSDLRect destRect)
  return ()

eventArrayStaticSize :: Int
eventArrayStaticSize = 128

-- Wrapper um das etwas eklige pollEvent
pollEvents :: IO [SDLT.Event]
pollEvents = allocaArray eventArrayStaticSize $ \eventArray -> do
  pumpEvents
  events <- peepEvents
    eventArray
    (fromIntegral eventArrayStaticSize)
    eventActionGetEvent
    eventTypeFirstEvent
    eventTypeLastEvent
  peekArray (fromEnum events) $ eventArray

fromSDLRect :: SDLT.Rect -> Rect (Point2 Int)
fromSDLRect (SDLT.Rect x y w h) = Rect (Point2 (fromIntegral x) (fromIntegral y)) (Point2 (fromIntegral (x+w)) (fromIntegral (y+h)))

toSDLRect :: Rect (Point2 Int) -> SDLT.Rect
toSDLRect (Rect (Point2 l t) (Point2 r b)) = SDLT.Rect (fromIntegral l) (fromIntegral t) (fromIntegral (r-l)) (fromIntegral (b-t))
