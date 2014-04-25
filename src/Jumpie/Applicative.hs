module Jumpie.Applicative(
  filterA
  ) where

import Control.Applicative(pure,(<*>),(<$>),Applicative)

filterA :: Applicative f => (a -> f Bool) -> [a] -> f [a]
filterA _ [] = pure []
filterA f (x:xs) = (\b -> if b then (x:) else id) <$> f x <*> filterA f xs
