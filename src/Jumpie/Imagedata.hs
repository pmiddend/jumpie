{-# LANGUAGE TupleSections,FlexibleContexts #-}
module Jumpie.Imagedata(
  ImageId,
  ImageMap,
  readAllDescFiles
  ) where

import Control.Monad(filterM,(>>=),(>>))
import Data.Function((.))
import Data.Eq((==))
import Data.Int(Int)
import Data.Map.Strict(fromList,union,empty)
import Jumpie.Types(RectInt,ImageId,ImageMap,ImageDescFile,SurfaceMap)
import Jumpie.Geometry.Point(Point2(Point2))
import Jumpie.Geometry.Rect(Rect(Rect))
import Jumpie.Parsec(int,safeParseFromFile)
import Graphics.UI.SDL.Types(Surface)
import Text.Parsec.Prim(Stream,ParsecT)
import Text.Parsec.Char(char,noneOf)
import Text.Parsec.Combinator(eof)
import Text.Parsec(many1)
import Data.Traversable(traverse)
import Jumpie.GameConfig(mediaDir)
import System.FilePath
import Graphics.UI.SDL.Image(load)
import Data.List(foldr,filter)
import System.Directory(getDirectoryContents,doesFileExist)
import Control.Applicative((<$>),(<*>),(*>),(<*))
import Control.Category((>>>))
import Prelude(Char)
import System.IO(IO,putStrLn)

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = getDirectoryContents dir >>= filterM (doesFileExist . (mediaDir </>))

-- Holt alle "Descriptorfiles" (also die mit .txt enden) aus dem Directory
getDescFilesInDir :: FilePath -> IO [ImageDescFile]
getDescFilesInDir dir = ((mediaDir </>) <$>) <$> filter (takeExtension >>> (== ".txt")) <$> (getFilesInDir dir)

readAllDescFiles :: IO SurfaceMap
readAllDescFiles = foldr union empty <$> smaps
  where readSingle :: ImageDescFile -> IO SurfaceMap
        readSingle f = imageDescToSurface f >>= imageDescToSurfaceMap f
        smaps :: IO [SurfaceMap]
        smaps = getDescFilesInDir mediaDir >>= traverse (\x -> putStrLn x >> readSingle x)

imageDescToSurface :: ImageDescFile -> IO Surface
imageDescToSurface x = load (replaceExtension x ".png")

imageDescToSurfaceMap :: ImageDescFile -> Surface -> IO SurfaceMap
imageDescToSurfaceMap f s = toSurfaceMap s <$> readImageData f

toSurfaceMap :: Surface -> ImageMap -> SurfaceMap
toSurfaceMap s = ((s,) <$>)

readImageData :: FilePath -> IO ImageMap
readImageData = safeParseFromFile imageDataC

imageDataC :: Stream s m Char => ParsecT s u m ImageMap
imageDataC = (fromList <$> many1 imageDataLineC) <* eof

point2IntC :: Stream s m Char => ParsecT s u m (Point2 Int)
point2IntC = Point2 <$> int <*> (char ',' *> int)

rect2IntC :: Stream s m Char => ParsecT s u m RectInt
rect2IntC = Rect <$> point2IntC <*> (char ',' *> point2IntC)

imageDataLineC :: Stream s m Char => ParsecT s u m (ImageId,RectInt)
imageDataLineC = (,) <$> (many1 (noneOf "=\n")) <*> (char '=' *> rect2IntC <* char '\n')
