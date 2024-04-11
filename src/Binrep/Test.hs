{-# LANGUAGE UndecidableInstances #-} -- for CBLen instances

module Binrep.Test where

import Binrep
import Binrep.Type.Magic
import GHC.Generics ( Generic )
import Data.Word
import Binrep.Util.ByteOrder

import Binrep.Common.Via.Generically.NonSum

data DMagic = DMagic
  { dMagic1_8b :: Magic '[0xFF, 0, 1, 0, 1, 0, 1, 0xFF]
  } deriving stock Generic
    deriving (IsCBLen, PutC) via GenericallyNonSum DMagic

data DMagicSum = DMagicSum1 (Magic '[0]) | DMagicSum2 (Magic '[0xFF])
    deriving stock Generic

data DStruct = DStruct
  { dStruct1 :: Magic '[0xFF, 0, 1, 0xFF]
  , dStruct2 :: ByteOrdered LE Word32
  , dStruct3 :: ()
  } deriving stock (Generic, Show)
    deriving (IsCBLen, PutC, GetC) via GenericallyNonSum DStruct
