module Jumpie.SDLHelper(
  putPixel32,
  createPixel,
  surfaceBresenham,
  fillSurface,
  blitAtPosition,
  pollEvents,
  fromSDLRect
  ) where

import Graphics.UI.SDL.Events(Event(NoEvent),pollEvent)
import Jumpie.Geometry.Point(Point2(Point2),_y,_x)
import Jumpie.Geometry.LineSegment(LineSegment)
import Jumpie.Geometry.Rect(Rect(Rect),dimensions)
import Jumpie.Monad(when_)
import Jumpie.Geometry.Intersection(lineSegmentInsideRect)
import Jumpie.Types(SurfaceData)
import Prelude((+),(*),RealFrac,floor,(-))
import qualified Graphics.UI.SDL.Rect as SDLR
import Graphics.UI.SDL.Types(Surface,surfaceGetPixelFormat,surfaceGetPixels,surfaceGetWidth)
import Data.Maybe(Maybe(..))
import Graphics.UI.SDL.Video(mapRGB,lockSurface,unlockSurface,getClipRect,fillRect,blitSurface)
import Graphics.UI.SDL.Color(Pixel(Pixel))
import Control.Monad(forM_,return,when)
import Jumpie.Geometry.Rect(inside)
import Data.Function(($))
import Data.Word(Word8)
import System.IO(IO)
import Control.Applicative((<$>))
import Data.Int(Int)
import Foreign.Ptr(castPtr)
import Foreign.Storable(pokeElemOff)
import Jumpie.Bresenham(bresenham)

createPixel :: Surface -> (Word8,Word8,Word8) -> IO Pixel
createPixel s (r,g,b) = mapRGB (surfaceGetPixelFormat s) r g b

putPixel32 :: Point2 Int -> Pixel -> Surface -> IO ()
putPixel32 p (Pixel pixel) s = do
  pixels <- castPtr <$> surfaceGetPixels s
  pokeElemOff pixels (((_y p) * surfaceGetWidth s) + (_x p)) pixel

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

blitAtPosition :: RealFrac a => SurfaceData -> Point2 a -> Surface -> IO ()
blitAtPosition (srcSurface,srcRect) pos destSurface = do
  scrClipRect <- fromSDLRect <$> getClipRect destSurface
  let destRect = Rect (floor <$> pos) (dimensions srcRect)
  when_ (destRect `inside` scrClipRect) $
    (blitSurface srcSurface (Just (toSDLRect srcRect)) destSurface (Just (toSDLRect destRect)))

-- Wrapper um das etwas eklige pollEvent
pollEvents :: IO [Event]
pollEvents = do
  event <- pollEvent
  case event of
    NoEvent -> return []
    _ -> do
      events <- pollEvents
      return $ event : events

fromSDLRect :: SDLR.Rect -> Rect (Point2 Int)
fromSDLRect (SDLR.Rect x y w h) = Rect (Point2 x y) (Point2 (x+w) (y+h))

toSDLRect :: Rect (Point2 Int) -> SDLR.Rect
toSDLRect (Rect (Point2 l t) (Point2 r b)) = SDLR.Rect l t (r-l) (b-t)
