module Binrep.Example.Tga where

import Binrep
import Binrep.Type.Derived.NullTermPadded
import Binrep.Type.AsciiNat
import Rerefined
import Strongweak
import Data.Word

data Header (s :: Strength) a = Header
  { idLen :: SW s Word8
  , colorMapType :: ColorMapType
  , imageType :: ImageType
  --, colorMapSpec :: 
  --, imageSpec
  }

data ColorMapType = NoColorMap {- ^ 0 -} | HasColorMap {- ^ 1 -}
data ImageType
  = NoImageData
  | UncompColorMapped
  | UncompTrueColor
  | UncompBW
  | RLEColorMapped
  | RLETrueColor
  | RLEBW
