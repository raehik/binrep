module Binrep.Util.ByteOrder
  ( ByteOrder(..), ByteOrdered(..)
  , type LE, type BE, type Endian
  ) where

import Raehik.Compat.Data.Primitive.Types.Endian ( ByteOrdered(..) )
import GHC.ByteOrder ( ByteOrder(..) )

-- shorter names I originally used
type LE = 'LittleEndian
type BE =    'BigEndian
type Endian = ByteOrdered
