module Jumpie.Time(
  TimeDelta(TimeDelta),
  timeDelta,
  GameTicks(GameTicks),
  tickValue,
  getTicks
  ) where

import Prelude(Double)
import Data.Word(Word64)
import System.IO(IO)
import System.Clock(Clock(Monotonic),getTime,TimeSpec(TimeSpec))
import Prelude(Double,undefined,fromIntegral,(-),(/),Fractional,div,error,floor,(+),(*),Integral,mod,abs)
import Control.Monad(return)
import Data.Function(($))
import Text.Show(Show)

newtype TimeDelta = TimeDelta { timeDelta :: Double } deriving(Show)

newtype GameTicks = GameTicks { tickValue :: Word64 } deriving(Show)

getTicks :: IO GameTicks
getTicks = do
  (TimeSpec s ns) <- getTime Monotonic
  return $ GameTicks $ ((fromIntegral s :: Word64) * 1000 * 1000 * 1000) + fromIntegral ns

