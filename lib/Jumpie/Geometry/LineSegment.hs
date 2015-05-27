{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Jumpie.Geometry.LineSegment(
  LineSegment(LineSegment),
  lineSegmentFrom,
  lineSegmentTo,
  pointList) where

import Data.Functor(Functor)
import Text.Show(Show)
import Data.Eq(Eq)
import Control.Lens.TH(makeLenses)

data LineSegment a = LineSegment {
    _lineSegmentFrom :: a
  , _lineSegmentTo :: a
  } deriving(Show,Eq,Functor)

$(makeLenses ''LineSegment)

pointList :: LineSegment a -> [a]
pointList (LineSegment a b) = [a,b]
