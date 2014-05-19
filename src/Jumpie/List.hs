module Jumpie.List(
  setPartList,
  replaceNth
  ) where

import Data.Int(Int)
import Data.List(take,replicate,drop,(++))
import Prelude((+),(-),otherwise)
import Data.Ord((<))

setPartList :: [a] -> (Int,Int) -> a -> [a]
setPartList xs (from,to) e | to < from = setPartList xs (to,from) e
                           | otherwise = (take from xs) ++ (replicate (to - from) e) ++ (drop to xs)

replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs i e = setPartList xs (i,i+1) e

