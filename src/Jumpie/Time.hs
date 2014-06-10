{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jumpie.Time(
  TimeDelta(TimeDelta),
  timeDelta,
  GameTicks(GameTicks),
  tickValue,
  getTicks,
  fromSeconds,
  fromNanoSeconds
  ) where

import           Control.Monad (return)
import           Data.Eq       (Eq)
import           Data.Function (($))
import           Data.Ord      (Ord)
import           Data.Word     (Word64)
import           Prelude       (Double, Num)
import           Prelude       (Double, Fractional, Integral, abs, div, error,
                                floor, fromIntegral, mod, undefined, (*), (+),
                                (-), (/))
import           System.Clock  (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import           System.IO     (IO)
import           Text.Show     (Show)

newtype TimeDelta = TimeDelta { timeDelta :: Double } deriving(Show)

newtype GameTicks = GameTicks { tickValue :: Word64 } deriving(Show,Num,Eq,Ord)

fromSeconds :: Num a => a -> GameTicks
fromSeconds s = GameTicks $ (fromIntegral s :: Word64) * 1000 * 1000 * 1000

fromNanoSeconds :: Num a => a -> GameTicks
fromNanoSeconds s = GameTicks (fromIntegral s)

getTicks :: IO GameTicks
getTicks = do
  (TimeSpec s ns) <- getTime Monotonic
  return $ fromSeconds s + fromNanoSeconds ns

