{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binrep.Util.ByteOrder
  ( ByteOrder(..), ByteOrdered(..), type EndianSuffix
  , type LE, type BE, type Endian
  ) where

import Raehik.Compat.Data.Primitive.Types.Endian ( ByteOrdered(..) )
import GHC.ByteOrder ( ByteOrder(..) )
import GHC.TypeLits ( type Symbol )

-- shorter names I originally used
type LE = LittleEndian
type BE =    BigEndian
type Endian = ByteOrdered

type family EndianSuffix (end :: ByteOrder) :: Symbol where
    EndianSuffix LittleEndian = "LE"
    EndianSuffix    BigEndian = "BE"
