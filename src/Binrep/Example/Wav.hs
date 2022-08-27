module Binrep.Example.Wav where

import Binrep
import Binrep.Generic
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.Magic

import GHC.Generics ( Generic )
import Data.Data ( Data )

type End = 'LE
type W32 = I 'U 'I4 End
type W16 = I 'U 'I2 End

data WavHeader = WavHeader
  { wavHeaderMagic :: Magic "RIFF"
  , wavHeaderChunkSize :: W32 -- file size - 8
  , wavHeaderFmt :: Magic "WAVE"
  , wavHeaderFmtChunkMarker :: Magic "fmt "
  , wavHeaderFmtType :: W16
  , wavHeaderChannels :: W16
  } deriving stock (Generic, Data, Show, Eq)

instance BLen WavHeader where blen = blenGeneric cNoSum
instance Put  WavHeader where put  = putGeneric  cNoSum
instance Get  WavHeader where get  = getGeneric  cNoSum
