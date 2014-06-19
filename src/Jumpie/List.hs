module Jumpie.List(
    setPartList
  , replaceNth
  , orEmptyTrue
  , inductiveFilter
  , withSuccessor
  , withPredecessor
  , countBy
  ) where

import Data.Int(Int)
import Data.List(take,replicate,drop,(++),or,elemIndices,map,length)
import Prelude((+),(-),otherwise)
import Data.Ord((<))
import Data.Bool(Bool(True))
import Data.Maybe(Maybe(..))
import Data.Function(($))

countBy :: (a -> Bool) -> [a] -> Int
countBy p xs = length $ elemIndices True $ (map p xs)

withSuccessor :: [a] -> [(a,Maybe a)]
withSuccessor [] = []
withSuccessor (x:[]) = [(x,Nothing)]
withSuccessor (x:y:xs) = (x,Just y) : withSuccessor (y:xs)

withPredecessor :: [a] -> [(Maybe a,a)]
withPredecessor [] = []
withPredecessor l@(a:_) = (Nothing,a) : withPredecessor' l
  where withPredecessor' [] = []
        withPredecessor' (_:[]) = []
        withPredecessor' (x:y:xs) = (Just x,y) : withPredecessor' xs

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
