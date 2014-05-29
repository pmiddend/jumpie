module Jumpie.List(
  setPartList,
  replaceNth,
  orEmptyTrue,
  inductiveFilter
  ) where

import Data.Int(Int)
import Data.List(take,replicate,drop,(++),or)
import Prelude((+),(-),otherwise)
import Data.Ord((<))
import Data.Bool(Bool(True))

orEmptyTrue :: [Bool] -> Bool
orEmptyTrue [] = True
orEmptyTrue xs = or xs

setPartList :: [a] -> (Int,Int) -> a -> [a]
setPartList xs (from,to) e | to < from = setPartList xs (to,from) e
                           | otherwise = (take from xs) ++ (replicate (to - from) e) ++ (drop to xs)

replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs i e = setPartList xs (i,i+1) e

inductiveFilter :: (a -> [a] -> Bool) -> [a] -> [a]
inductiveFilter f xs = inductiveFilter' [] xs f

inductiveFilter' :: [a] -> [a] -> (a -> [a] -> Bool) -> [a]
-- Verankerung fuer endliche Eingabelisten
inductiveFilter' _ [] _ = []
inductiveFilter' ns (x:xs) f = if f x ns
                               then x : (inductiveFilter' (x:ns) xs f)
                               else inductiveFilter' ns xs f
