module Jumpie.Random(
  randomElem,
  randomElemM
  ) where

import System.Random(RandomGen,randomR)
import Data.List((!!),length)
import Data.Tuple(fst)
import Prelude((-))
import Control.Monad.Random(MonadRandom,getRandomR)
import Control.Monad(return)
import Data.Function(($))

randomElem :: RandomGen g => g -> [a] -> a
randomElem g xs = xs !! (fst (randomR (0,length xs - 1) g))

randomElemM :: MonadRandom m => [a] -> m a
randomElemM xs = do
  index <- getRandomR (0,length xs - 1)
  return $ xs !! index
