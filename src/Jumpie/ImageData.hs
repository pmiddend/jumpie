{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module Jumpie.ImageData(
  ImageId,
  ImageMap,
  AnimMap,
  readAllDescFiles,
  animFrameSwitch,
  animFrames,
  SurfaceMap,
  SurfaceData
  ) where

import           Control.Applicative    ((*>), (<$>), (<*), (<*>))
import           Control.Category       ((>>>))
import           Control.Monad          (filterM, return, (>>=))
import           Data.Either            (Either (..))
import           Data.Eq                ((==))
import           Data.Function          (($), (.))
import           Data.Int               (Int)
import           Data.List              (filter, foldr, map, (++))
import           Data.Map.Strict        (empty, fromList, union)
import           Data.Map.Strict        (Map)
import           Data.Maybe             (Maybe (Just, Nothing), mapMaybe)
import           Data.String
import           Data.Traversable       (traverse)
import           Data.Tuple             (fst, snd)
import           Graphics.UI.SDL.Image  (imgLoadTexture)
import qualified Graphics.UI.SDL.Types  as SDLT
import           Jumpie.GameConfig      (mediaDir)
import           Jumpie.Geometry.Point  (Point2 (Point2))
import           Jumpie.Geometry.Rect   (Rect (Rect))
import           Jumpie.Parsec          (int, safeParseFromFile)
import           Jumpie.Types           (RectInt)
import           Prelude                (Char, error)
import           System.Directory       (doesFileExist, getDirectoryContents)
import           System.FilePath
import           System.IO              (IO)
import           Text.Parsec            (many1)
import           Text.Parsec.Char       (char, noneOf)
import           Text.Parsec.Combinator (eof, sepEndBy1)
import           Text.Parsec.Prim       (ParsecT, Stream, (<|>))

data DataLine = DataLineImage (ImageId,RectInt) | DataLineAnim (AnimId,Animation)

type ImageId = String

type ImageMap = Map ImageId RectInt

type ImageDescFile = FilePath

type SurfaceData = (SDLT.Texture,RectInt)

type SurfaceMap = Map ImageId SurfaceData

data Animation = Animation {
  animFrameSwitch :: Int,
  animFrames      :: [ImageId]
  }

type AnimId = String

type AnimMap = Map AnimId Animation

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = getDirectoryContents dir >>= filterM (doesFileExist . (mediaDir </>))

-- Holt alle "Descriptorfiles" (also die mit .txt enden) aus dem Directory
getDescFilesInDir :: FilePath -> IO [ImageDescFile]
getDescFilesInDir dir = ((mediaDir </>) <$>) <$> filter (takeExtension >>> (== ".txt")) <$> (getFilesInDir dir)

readAllDescFiles :: SDLT.Renderer -> IO (SurfaceMap,AnimMap)
readAllDescFiles r = (,) <$> (foldr union empty <$> smaps) <*> (foldr union empty <$> amaps)
  where readSingle :: ImageDescFile -> IO (SurfaceMap,AnimMap)
        readSingle f = imageDescToSurface r f >>= imageDescToMaps f
        maps :: IO [(SurfaceMap,AnimMap)]
        maps = getDescFilesInDir mediaDir >>= traverse readSingle
        smaps :: IO [SurfaceMap]
        smaps = map fst <$> maps
        amaps :: IO [AnimMap]
        amaps = map snd <$> maps

loadImage :: SDLT.Renderer -> FilePath -> IO SDLT.Texture
loadImage r f = do
  texture <- imgLoadTexture r f
  case texture of
    Left e -> error $ "Could not load image \"" ++ f ++ "\": " ++ e
    Right t -> return t

imageDescToSurface :: SDLT.Renderer -> ImageDescFile -> IO SDLT.Texture
imageDescToSurface r x = loadImage r (replaceExtension x ".png")

imageDescToMaps :: ImageDescFile -> SDLT.Texture -> IO (SurfaceMap,AnimMap)
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

toSurfaceMap :: SDLT.Texture -> ImageMap -> SurfaceMap
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
