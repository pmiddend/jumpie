{-# LANGUAGE OverloadedStrings #-}
module JumpieTestLevelGeneration(jumpieTestLevelGeneration) where

import Test.HUnit(Test(..),(@?=))
import ClassyPrelude
import Linear.V2
import Jumpie.LevelGeneration

jumpieTestLevelGeneration :: Test
jumpieTestLevelGeneration = TestList [jumpieTestLowerPlatform]

jumpieTestLowerPlatform :: Test
jumpieTestLowerPlatform = TestCase $ do
  platformReachable (Platform (V2 0 2) (V2 2 2)) (Platform (V2 6 8) (V2 8 8)) @?= True
