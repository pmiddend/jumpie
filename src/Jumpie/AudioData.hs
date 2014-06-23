module Jumpie.AudioData(

  ) where

import           Control.Monad         (mapM)
import           Data.Function         (($))
import           Data.List             (zip)
import           Data.Map.Strict       (Map, fromList)
import           Data.String           (String)
import           Graphics.UI.SDL.Mixer (Chunk)
import           Jumpie.Filesystem     (getFilesInDir)
import           System.FilePath

type SoundId = String

type SoundMap = Map SoundId Chunk

readAllSoundFiles :: IO SoundMap
readAllSoundFiles = do
  soundFiles <- getFilesInDir "sounds"
  loadedSounds <- mapM loadSound soundFiles
  return $ fromList $ zip (map (dropExtension . takeFileName) soundFiles) loadedSounds
