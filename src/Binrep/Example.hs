module Binrep.Example where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int

import GHC.Generics ( Generic )
import Data.Data ( Data )

brCfgNoSum :: BR.Cfg (I 'U 'I1 'LE)
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

data DV
    deriving stock (Generic, Data)

-- Disallowed. No binrepping void datatypes.
{-
instance BLen DV where blen = blenGeneric brCfgNoSum
instance Put  DV where put  = putGeneric  brCfgNoSum
instance Get  DV where get  = getGeneric  brCfgNoSum
-}

data DU = DU
    deriving stock (Generic, Data, Show, Eq)

instance BLen DU where blen = blenGeneric brCfgNoSum
instance Put  DU where put  = putGeneric  brCfgNoSum
instance Get  DU where get  = getGeneric  brCfgNoSum

data DSS = DSS
  { dss1 :: I 'U 'I1 'LE
  , dss2 :: I 'U 'I2 'LE
  , dss3 :: I 'U 'I4 'LE
  , dss4 :: I 'U 'I8 'LE
  , dss5 :: I 'U 'I1 'LE
  } deriving stock (Generic, Data, Show, Eq)

instance BLen DSS where blen = blenGeneric brCfgNoSum
instance Put  DSS where put  = putGeneric  brCfgNoSum
instance Get  DSS where get  = getGeneric  brCfgNoSum

data DCS = DCS1 {- DSS -} | DCS2 | DCS3 | DCS4 | DCS5
    deriving stock (Generic, Data, Show, Eq)

brCfgDCS :: BR.Cfg (I 'U 'I1 'LE)
brCfgDCS = BR.Cfg { BR.cSumTag = BR.cSumTagHex $ drop 3 }

--instance BR.BLen DCS where blen = BR.blenGeneric brCfgDCS
deriving anyclass instance BLen DCS
instance Put  DCS where put  = putGeneric  brCfgDCS
instance Get  DCS where get  = getGeneric  brCfgDCS

data DX = DX DU
    deriving stock (Generic, Data, Show, Eq)

type instance CBLen DX  = CBLenGeneric (I 'U 'I1 'LE) DX
type instance CBLen DU  = CBLenGeneric (I 'U 'I1 'LE) DU
type instance CBLen DSS = CBLenGeneric (I 'U 'I1 'LE) DSS
type instance CBLen DCS = CBLenGeneric (I 'U 'I1 'LE) DCS
deriving anyclass instance BLen DX

-- TODO clean up that CBLen messing around, probably don't mention it outside
-- the module (it's weird and will probably break things)
