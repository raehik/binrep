{-# LANGUAGE UndecidableInstances #-}

module Binrep.Test where

import Binrep
import Binrep.Type.Magic
import Binrep.CBLen.Generic
import GHC.Generics ( Generic )
import Data.Word
import Binrep.Util.ByteOrder

data DMagic = DMagic
  { dMagic1_8b :: Magic '[0xFF, 0, 1, 0, 1, 0, 1, 0xFF]
  } deriving stock Generic

instance IsCBLen DMagic where type CBLen DMagic = CBLenGenericNonSum DMagic
instance PutC DMagic where putC = putGenericStruct

data DMagicSum = DMagicSum1 (Magic '[0]) | DMagicSum2 (Magic '[0xFF])
    deriving stock Generic

instance IsCBLen DMagicSum where
    type CBLen DMagicSum = CBLenGenericNonSum DMagicSum

data DStruct = DStruct
  { dStruct1 :: Magic '[0xFF, 0, 1, 0xFF]
  , dStruct2 :: ByteOrdered LE Word32
  , dStruct3 :: ()
  } deriving stock (Generic, Show)

instance IsCBLen DStruct where type CBLen DStruct = CBLenGenericNonSum DStruct
instance GetC DStruct where getC = getGenericStruct
deriving via ViaGetC DStruct instance Get DStruct
