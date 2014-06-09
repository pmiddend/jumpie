{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck(quickCheck)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Jumpie.Geometry.Point(Point2(Point2),pX,pY)
import Jumpie.Geometry.Rect(Rect(Rect),rectTopLeft,rectBottomRight)
import Jumpie.List(setPartList,replaceNth,orEmptyTrue,withSuccessor,withPredecessor)
import Data.List(head,last)
import Jumpie.LevelGeneration(randomPlatform,Platform(Platform))
import Control.Applicative((<$>),(<*>))
import System.Random(mkStdGen)
import Data.Tuple(fst,snd)
import Data.Maybe(isNothing)

instance Arbitrary a => Arbitrary (Point2 a) where
  arbitrary = Point2 <$> arbitrary <*> arbitrary

instance Arbitrary (Rect (Point2 Int)) where
  arbitrary = do
    topLeft <- arbitrary
    bottomRight <- suchThat arbitrary (\(Point2 x y) -> x > pX topLeft && y > pY topLeft)
    return $ Rect topLeft bottomRight

prop_withSuccessor_lastNothing xs = not (null xs) ==> isNothing (snd (last (withSuccessor xs)))
prop_withPredecessor_headNothing xs = not (null xs) ==> isNothing (fst (head (withPredecessor xs)))

prop_platforms_length_bounded seed rect maxLength = maxLength > 0 ==> isBounded (randomPlatform (mkStdGen seed) rect maxLength)
  where isBounded ((Platform (Point2 left top) (Point2 right _)),_) = right - left <= maxLength

prop_platforms_bounded seed rect maxLength = maxLength > 0 ==> isBounded (randomPlatform (mkStdGen seed) rect maxLength)
  where isBounded ((Platform (Point2 left top) (Point2 right _)),_) = left >= ((pX . rectTopLeft) rect) && right <= ((pX . rectBottomRight) rect) && top >= ((pY . rectTopLeft) rect) && top <= ((pY . rectBottomRight) rect)

prop_replaceNth_core xs i e = i > 0 && i < length xs ==> (replaceNth xs i e) !! i == e

prop_replaceNth_preserve xs i e = length xs /= 0 && i > 0 && i < length xs ==> ((replaceNth xs i e) !! 0) == (xs !! 0)

prop_replaceNth_idem xs i e = i >= 0 && i < length xs ==> r == (replaceNth r i e)
  where r = replaceNth xs i e

prop_setPartList_begin xs i j e = i >= 0 && j > i && j < length xs ==> (setPartList xs (i,j) e) !! i == e
prop_setPartList_end xs i j e = i >= 0 && j > i && j < length xs ==> (setPartList xs (i,j) e) !! j == e

prop_setPartList_idem xs i j e = i >= 0 && j > i && j < length xs ==> r == setPartList r (i,j) e
  where r = setPartList xs (i,j) e

prop_orEmptyTrue xs = not (null xs) ==> orEmptyTrue xs == or xs

main :: IO ()
main = $defaultMainGenerator
