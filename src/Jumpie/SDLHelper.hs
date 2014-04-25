module Jumpie.SDLHelper(
  putPixel32,
  createPixel,
  surfaceBresenham,
  fillSurface,
  blitAtPosition,
  pollEvents) where

import Graphics.UI.SDL.Events(Event(NoEvent),pollEvent)
import Jumpie.Geometry.Point(Point2,_y,_x)
import Jumpie.Geometry.LineSegment(LineSegment)
import Prelude((+),(*),RealFrac,floor)
import Graphics.UI.SDL.Rect(Rect(..))
import Graphics.UI.SDL.Types(Surface,surfaceGetPixelFormat,surfaceGetPixels,surfaceGetWidth)
import Data.Maybe(Maybe(..))
import Graphics.UI.SDL.Video(mapRGB,lockSurface,unlockSurface,getClipRect,fillRect,blitSurface)
import Graphics.UI.SDL.Color(Pixel(Pixel))
import Control.Monad(forM_,return)
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

blitAtPosition :: RealFrac a => Surface -> Surface -> Point2 a -> IO ()
blitAtPosition sourceSurface destSurface pos = do
  clipRect <- getClipRect sourceSurface
  let destRect = Rect (floor (_x pos)) (floor (_y pos)) (rectW clipRect) (rectH clipRect)
  _ <- blitSurface sourceSurface Nothing destSurface (Just destRect)
  return ()

-- Wrapper um das etwas eklige pollEvent
pollEvents :: IO [Event]
pollEvents = do
  event <- pollEvent
  case event of
    NoEvent -> return []
    _ -> do
      events <- pollEvents
      return $ event : events
