{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jumpie.Time(
  {-
  TimeDelta(TimeDelta),
  timeDelta,
  GameTicks(GameTicks),
  tickValue,
  getTicks,
  fromSeconds,
  fromNanoSeconds,
  tickDelta
-}
  ) where

import           Control.Monad (return)
import           Data.Eq       (Eq)
import           Data.Function (($))
import           Data.Ord      (Ord)
import           Data.Word     (Word64)
import           Prelude       (Double, Enum, Fractional, Integral, Num, Real,
                                abs, div, error, floor, fromIntegral, mod,
                                undefined, (*), (+), (-), (/))
import           System.Clock  (Clock (Monotonic), TimeSpec (TimeSpec), getTime)
import           System.IO     (IO)
import           Text.Show     (Show)

newtype TimeDelta = TimeDelta { timeDelta :: Double } deriving(Show,Num,Eq,Ord)

newtype GameTicks = GameTicks { tickValue :: Word64 } deriving(Show,Num,Eq,Ord,Integral,Real,Enum)

fromSeconds :: Integral a => a -> GameTicks
fromSeconds s = GameTicks $ (fromIntegral s :: Word64) * 1000 * 1000 * 1000

fromNanoSeconds :: Integral a => a -> GameTicks
fromNanoSeconds s = GameTicks (fromIntegral s)

tickDelta :: GameTicks -> GameTicks -> TimeDelta
tickDelta newTicks oldTicks = TimeDelta $ fromIntegral (tickValue newTicks - tickValue oldTicks) / (1000.0 * 1000.0 * 1000.0)

getTicks :: IO GameTicks
getTicks = do
  (TimeSpec s ns) <- getTime Monotonic
  return $ fromSeconds s + fromNanoSeconds ns

