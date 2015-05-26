module Main where

import           System.Random              (getStdGen)
import qualified Jumpie.LevelGeneration as LG
import ClassyPrelude
import           Control.Monad.Writer.Strict       (runWriterT)
import           Control.Monad.Random       (evalRandT)
import Jumpie.Geometry.Rect
import Linear.V2

main :: IO ()
main = do
  g <- getStdGen
  let
    boundingBox = Rect (V2 0 0) (V2 30 10)
    maxLen = 5
    plats = do
      startPlat <- LG.newLevelGen (0,9) maxLen []
      nextPlats <- LG.newLevelGen (0,9) maxLen startPlat
      nextPlats' <- LG.newLevelGen (0,9) maxLen nextPlats
      return (startPlat <> nextPlats <> nextPlats')
    --plats = [LG.Platform (V2 2 2) (V2 10 2)]  
  (r,logLines) <- runWriterT (evalRandT plats g)
  mapM_ putStrLn logLines
  putStrLn (pack (LG.showPlatformsPpm boundingBox r))
