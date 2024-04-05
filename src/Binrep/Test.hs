{-# LANGUAGE UndecidableInstances #-}

module Binrep.Test where

import Binrep
import Binrep.Type.Magic
import Binrep.CBLen.Generic
import GHC.Generics ( Generic )

data DMagic = DMagic
  { dMagic1_8b :: Magic '[0xFF, 0, 1, 0, 1, 0, 1, 0xFF]
  } deriving stock Generic

instance IsCBLen DMagic where type CBLen DMagic = CBLenGeneric () DMagic
instance PutC DMagic where putC = putGenericStruct
