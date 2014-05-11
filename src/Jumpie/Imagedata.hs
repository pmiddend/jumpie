{-# LANGUAGE TupleSections,FlexibleContexts #-}
module Jumpie.Imagedata(
  ImageId,
  ImageMap,
  readAllDescFiles
  ) where

import Control.Monad(filterM,(>>=))
import Data.Maybe(mapMaybe,Maybe(Just,Nothing))
import Data.Function((.))
import Data.Eq((==))
import Data.Int(Int)
import Data.Map.Strict(fromList,union,empty)
import Jumpie.Types(RectInt,ImageId,ImageMap,ImageDescFile,SurfaceMap,Animation(Animation),AnimMap,AnimId)
import Jumpie.Geometry.Point(Point2(Point2))
import Jumpie.Geometry.Rect(Rect(Rect))
import Jumpie.Parsec(int,safeParseFromFile)
import Graphics.UI.SDL.Types(Surface)
import Text.Parsec.Prim(Stream,ParsecT,(<|>))
import Text.Parsec.Char(char,noneOf)
import Text.Parsec.Combinator(eof,sepEndBy1)
import Text.Parsec(many1)
import Data.Traversable(traverse)
import Jumpie.GameConfig(mediaDir)
import System.FilePath
import Graphics.UI.SDL.Image(load)
import Data.List(foldr,filter,map)
import System.Directory(getDirectoryContents,doesFileExist)
import Control.Applicative((<$>),(<*>),(*>),(<*))
import Control.Category((>>>))
import Prelude(Char)
import Data.Tuple(fst,snd)
import System.IO(IO)

data DataLine = DataLineImage (ImageId,RectInt) | DataLineAnim (AnimId,Animation)

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = getDirectoryContents dir >>= filterM (doesFileExist . (mediaDir </>))

-- Holt alle "Descriptorfiles" (also die mit .txt enden) aus dem Directory
getDescFilesInDir :: FilePath -> IO [ImageDescFile]
getDescFilesInDir dir = ((mediaDir </>) <$>) <$> filter (takeExtension >>> (== ".txt")) <$> (getFilesInDir dir)

readAllDescFiles :: IO (SurfaceMap,AnimMap)
readAllDescFiles = (,) <$> (foldr union empty <$> smaps) <*> (foldr union empty <$> amaps)
  where readSingle :: ImageDescFile -> IO (SurfaceMap,AnimMap)
        readSingle f = imageDescToSurface f >>= imageDescToMaps f
        maps :: IO [(SurfaceMap,AnimMap)]
        maps = getDescFilesInDir mediaDir >>= traverse readSingle
        smaps :: IO [SurfaceMap]
        smaps = map fst <$> maps
        amaps :: IO [AnimMap]
        amaps = map snd <$> maps

imageDescToSurface :: ImageDescFile -> IO Surface
imageDescToSurface x = load (replaceExtension x ".png")

imageDescToMaps :: ImageDescFile -> Surface -> IO (SurfaceMap,AnimMap)
imageDescToMaps f s = (,) <$> (toSurfaceMap s <$> rSurfaceData) <*> rAnimData
  where rImageData :: IO [DataLine]
        rImageData = readImageData f
        rSurfaceData :: IO ImageMap
        rSurfaceData = fromList <$> (mapMaybe (\x -> case x of
                                  DataLineImage i -> Just i
                                  _ -> Nothing) <$> rImageData)
        rAnimData :: IO AnimMap
        rAnimData = fromList <$> (mapMaybe (\x -> case x of
                                  DataLineAnim i -> Just i
                                  _ -> Nothing) <$> rImageData)

toSurfaceMap :: Surface -> ImageMap -> SurfaceMap
toSurfaceMap s = ((s,) <$>)

readImageData :: FilePath -> IO [DataLine]
readImageData = safeParseFromFile imageDataC

imageDataC :: Stream s m Char => ParsecT s u m [DataLine]
imageDataC = (sepEndBy1 imageDataLineC (char '\n')) <* eof

point2IntC :: Stream s m Char => ParsecT s u m (Point2 Int)
point2IntC = Point2 <$> int <*> (char ',' *> int)

rect2IntC :: Stream s m Char => ParsecT s u m RectInt
rect2IntC = Rect <$> point2IntC <*> (char ',' *> point2IntC)

imageDataLineC :: Stream s m Char => ParsecT s u m DataLine
imageDataLineC = (char '>' *> (DataLineAnim <$> imageDataLineAnimC)) <|> (DataLineImage <$> imageDataLineImageC)

imageDataLineImageC :: Stream s m Char => ParsecT s u m (ImageId,RectInt)
imageDataLineImageC = (,) <$> (many1 (noneOf "=\n")) <*> (char '=' *> rect2IntC)

imageDataLineAnimC :: Stream s m Char => ParsecT s u m (AnimId,Animation)
imageDataLineAnimC = (,) <$> (many1 (noneOf "=\n")) <*> (Animation <$> (char '=' *> int <* char '|') <*> (sepEndBy1 (many1 (noneOf ",\n")) (char ',')))
