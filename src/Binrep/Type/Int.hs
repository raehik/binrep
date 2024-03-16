-- | "Machine" integers: sized integers & naturals with explicit endianness type
--   tags for serialization.

{-# LANGUAGE CPP #-} -- for host endianness checking
{-# LANGUAGE UndecidableInstances #-} -- for convenient type level arithmetic

module Binrep.Type.Int where

import Binrep.Put ( Put(put) )
import Bytezap.Write qualified as BZ
import Data.Primitive.Types qualified as Prim
import Data.Primitive.Types ( Prim )
import Raehik.Compat.Data.Primitive.Types ( Prim' )
import Binrep.Util.ByteOrder
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )

import Binrep.BLen.Simple qualified as Simple
import Binrep.CBLen

import Binrep.Get.Flatparse qualified as Flatparse
import FlatParse.Basic qualified as FP

import Strongweak

import Data.Word
import Data.Int
import Data.Aeson

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Binrep.Via ( Binreply(..) )

-- | Endianness doesn't matter for single bytes.
deriving via Binreply Word8 instance Flatparse.Get (ByteOrdered end Word8)

-- | Endianness doesn't matter for single bytes.
deriving via Binreply  Int8 instance Flatparse.Get (ByteOrdered end  Int8)

-- | Ask for a minimum length before running the given parser and wrapping the
--   result in 'ByteOrdered'.
flatparseParseEndianMin
    :: forall a end. Prim a
    => Flatparse.Getter a -> Flatparse.Getter (ByteOrdered end a)
flatparseParseEndianMin f =
    ByteOrdered <$> Flatparse.getEBase f (Flatparse.ERanOut n)
  where n = Prim.sizeOf (undefined :: a)

instance Flatparse.Get (Endian LE Word16) where
    get = flatparseParseEndianMin FP.anyWord16le
instance Flatparse.Get (Endian BE Word16) where
    get = flatparseParseEndianMin FP.anyWord16be

instance Flatparse.Get (Endian LE Word32) where
    get = flatparseParseEndianMin FP.anyWord32le
instance Flatparse.Get (Endian BE Word32) where
    get = flatparseParseEndianMin FP.anyWord32be

instance Flatparse.Get (Endian LE Word64) where
    get = flatparseParseEndianMin FP.anyWord64le
instance Flatparse.Get (Endian BE Word64) where
    get = flatparseParseEndianMin FP.anyWord64be

instance Flatparse.Get (Endian LE Int16) where
    get = flatparseParseEndianMin FP.anyInt16le
instance Flatparse.Get (Endian BE Int16) where
    get = flatparseParseEndianMin FP.anyInt16be

instance Flatparse.Get (Endian LE Int32) where
    get = flatparseParseEndianMin FP.anyInt32le
instance Flatparse.Get (Endian BE Int32) where
    get = flatparseParseEndianMin FP.anyInt32be

instance Flatparse.Get (Endian LE Int64) where
    get = flatparseParseEndianMin FP.anyInt64le
instance Flatparse.Get (Endian BE Int64) where
    get = flatparseParseEndianMin FP.anyInt64be
