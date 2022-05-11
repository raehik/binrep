module Binrep.Example.Tar where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.Magic
import Binrep.Type.Byte
import Binrep.Predicate.NullPad

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )
import GHC.TypeLits

import Data.ByteString qualified as B

type E = 'LE
type W32 = I 'U 'I4 E
type W16 = I 'U 'I2 E
type BS = B.ByteString

data Tar = Tar
  { tarFile :: NullPadded 100 BS
  }
