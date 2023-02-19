module Binrep.Type.Common where

import GHC.Generics ( Generic )
import Data.Data ( Data )

-- | Byte order.
data Endianness
  = LE -- ^ little endian, MSB last.  e.g. most processor architectures
  | BE -- ^    big endian, MSB first. e.g. most network protocols
    deriving stock (Generic, Data, Show, Eq)
