module Binrep.Type.Common where

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

-- | Byte order.
data Endianness
  = BE -- ^    big endian, MSB first. e.g. most network protocols
  | LE -- ^ little endian, MSB last.  e.g. most processor architectures
    deriving stock (Generic, Typeable, Data, Show, Eq)
