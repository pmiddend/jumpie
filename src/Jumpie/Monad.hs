module Jumpie.Monad(
  when_,
  concatMapM) where

import           Control.Monad (Monad, liftM, mapM, return, when, (>>))
import           Data.Bool     (Bool)
import           Data.List     (concat)

when_ :: Monad m => Bool -> m a -> m ()
when_ b a = when b (a >> return ())

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)
