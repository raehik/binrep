module Binrep.Example where

import Binrep qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

brCfgNoSum :: BR.Cfg (I 'U 'I1 'LE)
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

data DV
    deriving stock (Generic, Typeable, Data)

-- Disallowed. No binrepping void datatypes.
{-
instance BR.BLen DV where blen = BR.blenGeneric brCfgNoSum
instance BR.Put  DV where put  = BR.putGeneric  brCfgNoSum
instance BR.Get  DV where get  = BR.getGeneric  brCfgNoSum
-}

data DU = DU
    deriving stock (Generic, Typeable, Data, Show, Eq)

instance BR.BLen DU where blen = BR.blenGeneric brCfgNoSum
instance BR.Put  DU where put  = BR.putGeneric  brCfgNoSum
instance BR.Get  DU where get  = BR.getGeneric  brCfgNoSum

data DSS = DSS
  { dss1 :: I 'U 'I1 'LE
  , dss2 :: I 'U 'I2 'LE
  , dss3 :: I 'U 'I4 'LE
  , dss4 :: I 'U 'I8 'LE
  , dss5 :: I 'U 'I1 'LE
  } deriving stock (Generic, Typeable, Data, Show, Eq)

instance BR.BLen DSS where blen = BR.blenGeneric brCfgNoSum
instance BR.Put  DSS where put  = BR.putGeneric  brCfgNoSum
instance BR.Get  DSS where get  = BR.getGeneric  brCfgNoSum

data DCS = DCS1 {- DSS -} | DCS2 | DCS3 | DCS4 | DCS5
    deriving stock (Generic, Typeable, Data, Show, Eq)

brCfgDCS :: BR.Cfg (I 'U 'I1 'LE)
brCfgDCS = BR.Cfg { BR.cSumTag = BR.cSumTagHex $ drop 3 }

--instance BR.BLen DCS where blen = BR.blenGeneric brCfgDCS
deriving anyclass instance BR.BLen DCS
instance BR.Put  DCS where put  = BR.putGeneric  brCfgDCS
instance BR.Get  DCS where get  = BR.getGeneric  brCfgDCS

data DX = DX DU
    deriving stock (Generic, Typeable, Data, Show, Eq)

type instance BR.CBLen DX  = BR.CBLenGeneric (I 'U 'I1 'LE) DX
type instance BR.CBLen DU  = BR.CBLenGeneric (I 'U 'I1 'LE) DU
type instance BR.CBLen DSS = BR.CBLenGeneric (I 'U 'I1 'LE) DSS
type instance BR.CBLen DCS = BR.CBLenGeneric (I 'U 'I1 'LE) DCS
deriving anyclass instance BR.BLen DX

-- TODO clean up that CBLen messing around, probably don't mention it outside
-- the module (it's weird and will probably break things)
