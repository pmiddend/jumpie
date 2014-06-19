module Jumpie.SDLHelper(
  {-
  putPixel32,
  createPixel,
  surfaceBresenham,
  fillSurface,
-}
    blitSameSize
  , blitRescale
  , pollEvents
  , fromSDLRect
  , withWindow
  , withRenderer
  , renderFinish
  , processKeydowns
  , renderClear
  ) where

import           Control.Monad         (return, (>>))
import           Data.Bool             (Bool (..))
import           Data.Function         (($), (.))
import           Data.Int              (Int)
import           Data.List             (filter, map, union, (++), (\\))
import           Data.Tuple            (fst)
import           Foreign.Marshal.Array (allocaArray, peekArray)
import           Foreign.Marshal.Utils (with)
import           Graphics.UI.SDL.Enum  (eventActionGetEvent,
                                        eventTypeFirstEvent, eventTypeKeyDown,
                                        eventTypeKeyUp, eventTypeLastEvent,
                                        windowFlagResizable)
import           Graphics.UI.SDL.Event (peepEvents, pumpEvents)
import qualified Graphics.UI.SDL.Types as SDLT
import qualified Graphics.UI.SDL.Video as SDLV
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
import           Data.Eq               (Eq, (==))
import           Data.String           (String)
import           Foreign.C.String      (withCStringLen)
import           Graphics.UI.SDL.Enum  (windowPosUndefined, windowPosUndefined)
import           Graphics.UI.SDL.Types (Renderer, Window)
import           Graphics.UI.SDL.Types (Event (KeyboardEvent))
import           Jumpie.Types          (Keydowns, PointInt, RectInt)
import           Prelude               (Num, RealFrac, error, floor, fromEnum,
                                        fromIntegral, undefined, (*), (+), (-))
import           System.IO             (IO)

processKeydowns :: Keydowns -> [Event] -> Keydowns
processKeydowns k es = (k \\ keyUps) `union` keyDowns
  where keyUps = (map toKey  . filter isKeyUp) es
        keyDowns = (map toKey  . filter isKeyDown) es
        isKeyUp (KeyboardEvent state _ _ _ _ _) = state == eventTypeKeyUp
        isKeyUp _ = False
        isKeyDown (KeyboardEvent state _ _ _ _ _) = state == eventTypeKeyDown
        isKeyDown _ = False
        toKey e = case e of
          KeyboardEvent _ _ _ _ _ (SDLT.Keysym l _ _) -> l
          _ -> undefined

renderClear :: Renderer -> IO ()
renderClear renderer = SDLV.renderClear renderer >> return ()

screenAbsoluteWidth,screenAbsoluteHeight :: Int
screenAbsoluteWidth = 0
screenAbsoluteHeight = 0
windowFlags :: Int
windowFlags = windowFlagResizable

errorIfNonZero :: (Num a,Eq a) => IO a -> String -> IO ()
errorIfNonZero action s = do
  result <- action
  if result == 0 then return () else error $ s ++ " returned non-zero"

renderFinish :: Renderer -> IO ()
renderFinish renderer = SDLV.renderPresent renderer

withWindow :: String -> (Window -> IO a) -> IO a
withWindow title callback = withCStringLen title $ \windowTitle ->
  let acquireResource = SDLV.createWindow (fst windowTitle) windowPosUndefined windowPosUndefined (fromIntegral screenAbsoluteWidth) (fromIntegral screenAbsoluteHeight) (fromIntegral windowFlags)
      releaseResource = SDLV.destroyWindow
  in bracket acquireResource releaseResource callback

withRenderer :: Window -> Int -> Int -> (Renderer -> IO a) -> IO a
withRenderer window screenWidth screenHeight callback =
  let acquireResource = SDLV.createRenderer window (-1) 0
      releaseResource = SDLV.destroyRenderer
  in bracket acquireResource releaseResource $ \renderer -> do
    errorIfNonZero (SDLV.renderSetLogicalSize renderer (fromIntegral screenWidth) (fromIntegral screenHeight)) "renderSetLogicalSize"
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
renderSprite :: SDLT.Renderer -> SDLT.Texture -> SDLT.Rect -> SDLT.Rect -> IO ()
renderSprite r t srcrect dstrect = with srcrect $ \srcrectptr -> with dstrect $ \dstrectptr ->
  errorIfNonZero (SDLV.renderCopy r t srcrectptr dstrectptr) "renderCopy"

blitRescale :: SurfaceData -> RectInt -> SDLT.Renderer -> IO ()
blitRescale (srcSurface,srcRect) destRect renderer = do
  renderSprite renderer srcSurface (toSDLRect srcRect) (toSDLRect destRect)

blitSameSize :: SurfaceData -> PointInt -> SDLT.Renderer -> IO ()
blitSameSize sd@(_,srcRect) pos renderer =
  blitRescale sd (Rect pos (pos + (dimensions srcRect))) renderer

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
