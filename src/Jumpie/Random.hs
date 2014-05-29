module Jumpie.Random(
  randomElem
  ) where

import System.Random(RandomGen,randomR)
import Data.List((!!),length)
import Data.Tuple(fst)

randomElem :: RandomGen g => g -> [a] -> a
randomElem g xs = xs !! (fst (randomR (0,length xs - 1) g))
