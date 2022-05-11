module Binrep.Example.Wav where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.Magic
import Binrep.Type.Byte

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )
import GHC.TypeLits

type E = 'LE
type W32 = I 'U 'I4 E
type W16 = I 'U 'I2 E

data WavHeader = WavHeader
  { wavHeaderMagic :: Magic "RIFF"
  , wavHeaderChunkSize :: W32 -- file size - 8
  , wavHeaderFmt :: Magic "WAVE"
  , wavHeaderFmtChunkMarker :: Magic "fmt "
  , wavHeaderFmtType :: W16
  , wavHeaderChannels :: W16
  }
