module Jumpie.Monad(
  when_) where

import Control.Monad(Monad,(>>),return,when)
import Data.Bool(Bool)
import Prelude()

when_ :: Monad m => Bool -> m a -> m ()
when_ b a = when b (a >> return ())
