module Jumpie.Imagedata(
                       ) where

import Data.String(String)
import Prelude()
import Jumpie.Types(RectInt)
import Data.Aeson(FromJSON,parseJSON)

newtype ImageId = ImageId { getImageId :: String }
newtype AnimId = AnimId { getAnimId :: String }

type ImageMap = [(ImageId,RectInt)]
type AnimMap = [(AnimId,RectInt)]

data Imagedata = Imagedata {
  images :: ImageMap,
  anims :: AnimMap
  }

instance FromJSON Imagedata where
  parseJSON (Object o) = Imagedata 
  parseJSON _ = mzero
