module Binrep.Example.Wav where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.Magic

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

type E = 'LE
type W32 = I 'U 'I4 E
type W16 = I 'U 'I2 E

brCfgNoSum :: BR.Cfg (I 'U 'I1 'LE)
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

data WavHeader = WavHeader
  { wavHeaderMagic :: Magic "RIFF"
  , wavHeaderChunkSize :: W32 -- file size - 8
  , wavHeaderFmt :: Magic "WAVE"
  , wavHeaderFmtChunkMarker :: Magic "fmt "
  , wavHeaderFmtType :: W16
  , wavHeaderChannels :: W16
  } deriving stock (Generic, Typeable, Data, Show, Eq)

instance BLen WavHeader where blen = blenGeneric brCfgNoSum
instance Put  WavHeader where put  = putGeneric  brCfgNoSum
instance Get  WavHeader where get  = getGeneric  brCfgNoSum
