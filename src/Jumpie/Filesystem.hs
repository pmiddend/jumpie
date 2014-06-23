module Jumpie.Filesystem(
  getFilesInDir
  ) where

import           Control.Monad     (filterM, (>>=))
import           Data.Function     ((.))
import           Jumpie.GameConfig (mediaDir)
import           System.Directory  (doesFileExist, getDirectoryContents)
import           System.FilePath
import           System.IO         (IO)

-- Holt nur die Files (ohne . und ..) aus einem Verzeichnis
getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir dir = getDirectoryContents dir >>= filterM (doesFileExist . (mediaDir </>))
