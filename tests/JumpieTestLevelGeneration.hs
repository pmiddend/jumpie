{-# LANGUAGE OverloadedStrings #-}
module JumpieTestLevelGeneration(jumpieTestLevelGeneration) where

import Test.HUnit(Test(..),(@?=),assertFailure)
import ClassyPrelude
import Linear.V2
import Jumpie.LevelGeneration
import Jumpie.GameConfig

jumpieTestLevelGeneration :: Test
jumpieTestLevelGeneration = TestList [jumpieTestLowerPlatform,jumpieTestReachablePlatform,jumpieTestDistance,jumpieTestMaxJumpWidth,jumpieTestMaxJumpHeight]

jumpieTestLowerPlatform :: Test
jumpieTestLowerPlatform = TestCase $ do
  platformReachable (Platform (V2 0 2) (V2 2 2)) (Platform (V2 6 8) (V2 8 8)) @?= False

jumpieTestMaxJumpWidth :: Test
jumpieTestMaxJumpWidth = TestCase $ do
  let jumpWidthTiles = floor (playerMaxJumpWidth/fromIntegral gcTileSize)
  if jumpWidthTiles < 5
    then assertFailure $ "Player jump width is " <> show jumpWidthTiles
    else return ()

jumpieTestMaxJumpHeight :: Test
jumpieTestMaxJumpHeight = TestCase $ do
  let jumpHeightTiles = floor (playerMaxJumpHeight/fromIntegral gcTileSize)
  if jumpHeightTiles < 2
    then assertFailure $ "Player jump height is " <> show jumpHeightTiles
    else return ()

jumpieTestDistance :: Test
jumpieTestDistance = TestCase $ do
  platformManhattanDistance (Platform (V2 0 1) (V2 1 1)) (Platform (V2 3 2) (V2 6 2)) @?= V2 2 1

jumpieTestReachablePlatform :: Test
jumpieTestReachablePlatform = TestCase $ do
  platformReachable (Platform (V2 0 1) (V2 1 1)) (Platform (V2 3 2) (V2 6 2)) @?= True
