{-# LANGUAGE UndecidableInstances #-}

module Binrep.Example where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Void ( Void )

data DV
    deriving stock (Generic, Data)

-- Disallowed. No binrepping void datatypes.
{-
instance BLen DV where blen = blenGeneric BR.cNoSum
instance Put  DV where put  = putGeneric  BR.cNoSum
instance Get  DV where get  = getGeneric  BR.cNoSum
-}

data DU = DU
    deriving stock (Generic, Data, Show, Eq)

--instance BLen DU where blen = blenGeneric BR.cNoSum
instance BLen DU where type CBLen DU = CBLenGeneric Void DU
instance Put  DU where put  = putGeneric  cNoSum
instance Get  DU where get  = getGeneric  cNoSum

data DSS = DSS
  { dss1 :: I 'U 'I1 'LE
  , dss2 :: I 'U 'I2 'LE
  , dss3 :: I 'U 'I4 'LE
  , dss4 :: I 'U 'I8 'LE
  , dss5 :: I 'U 'I1 'LE
  } deriving stock (Generic, Data, Show, Eq)

instance BLen DSS where blen = blenGeneric cNoSum
--instance BLen DSS where type CBLen DSS = CBLenGeneric Void DSS
instance Put  DSS where put  = putGeneric  cNoSum
instance Get  DSS where get  = getGeneric  cNoSum

data DCS = DCS1 {- DSS -} | DCS2 | DCS3 | DCS4 | DCS5
    deriving stock (Generic, Data, Show, Eq)

type BrSumDCS = I 'U 'I1 'LE
brCfgDCS :: BR.Cfg BrSumDCS
brCfgDCS = BR.cfg $ BR.cSumTagHex $ drop 3

--instance BLen DCS where blen = BR.blenGeneric brCfgDCS
instance BLen DCS where type CBLen DCS = CBLenGeneric BrSumDCS DCS
instance Put  DCS where put  = putGeneric  brCfgDCS
instance Get  DCS where get  = getGeneric  brCfgDCS

data DX = DX DU
    deriving stock (Generic, Data, Show, Eq)

--instance BLen DX where blen = blenGeneric brCfgNoSum
instance BLen DX where type CBLen DX = CBLenGeneric Void DX
instance Put  DX where put  = putGeneric  cNoSum
instance Get  DX where get  = getGeneric  cNoSum
