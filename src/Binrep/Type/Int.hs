-- | "Machine" integers: sized integers & naturals with explicit endianness type
--   tags for serialization.

{-# LANGUAGE CPP #-} -- for host endianness checking
{-# LANGUAGE UndecidableInstances #-} -- for convenient type level arithmetic

module Binrep.Type.Int where

import Binrep

import Bytezap.Int qualified as BZ
import FlatParse.Basic qualified as FP

import Binrep.Type.Common ( Endianness(..) )
import Strongweak

import Data.Word
import Data.Int
import Data.Aeson

import GHC.Generics ( Generic )
import Data.Data ( Data )
import GHC.TypeNats

import Binrep.Via ( Binreply(..) )

-- | Machine integer sign.
--
-- Signed integers use two's complement for representation.
data ISign
  = U -- ^ unsigned
  | I -- ^   signed (two's complement)
    deriving stock (Generic, Data, Show, Eq)

-- | A type tagged with the endianness (byte order) to use when serializing.
--
-- Intended to be used to wrap existing types which do not otherwise expose
-- endianness, namely the machine integers 'Int32', 'Word64' etc. As such, it
-- derives various relevant type classes using the wrapped type.
--
-- May be considered a restricted 'Data.Tagged.Tagged' (from the @tagged@
-- package).
newtype Endian (end :: Endianness) a = Endian
    { -- | Discard endianness information.
      unEndian :: a }
    deriving stock (Generic, Data, Show)
    deriving (Eq, Ord, Bounded, Num, Enum, Real, Integral) via a
    deriving (IsCBLen, BLen) via a
    deriving (Weaken, Strengthen) via a
    deriving (ToJSON, FromJSON) via a

-- | Endianness doesn't matter for single bytes.
deriving via Binreply Word8 instance Put (Endian end Word8)

-- | Endianness doesn't matter for single bytes.
deriving via Binreply Word8 instance Get (Endian end Word8)

-- | Endianness doesn't matter for single bytes.
deriving via Binreply  Int8 instance Put (Endian end  Int8)

-- | Endianness doesn't matter for single bytes.
deriving via Binreply  Int8 instance Get (Endian end  Int8)

{- 2023-02-01 raehik:
byteswapping should be faster than poking "backwards" byte by byte. confirmed
with nikita-volkov's ptr-poker package via benchmarks (single operation ~2%
faster) and inspecting disassembly (byteswapX is inlined at the assembly level
as BSWAP, byte by byte takes lots of MOVs and SHRs)

2023-02-18 raehik: this change is applied to nikita-volkov's ptr-poker pkg :)
-}

-- | Ask for a minimum length before running the given parser and wrapping the
--   result in 'Endian'.
parseEndianMin :: Getter a -> Int -> Getter (Endian end a)
parseEndianMin f n = Endian <$> getEBase f (ERanOut n)

instance Put (Endian 'LE Word16) where put = BZ.w16le . unEndian
instance Get (Endian 'LE Word16) where
    get = parseEndianMin FP.anyWord16le 2
instance Put (Endian 'BE Word16) where put = BZ.w16be . unEndian
instance Get (Endian 'BE Word16) where
    get = parseEndianMin FP.anyWord16be 2

instance Put (Endian 'LE Word32) where put = BZ.w32le . unEndian
instance Get (Endian 'LE Word32) where
    get = parseEndianMin FP.anyWord32le 4
instance Put (Endian 'BE Word32) where put = BZ.w32be . unEndian
instance Get (Endian 'BE Word32) where
    get = parseEndianMin FP.anyWord32be 4

instance Put (Endian 'LE Word64) where put = BZ.w64le . unEndian
instance Get (Endian 'LE Word64) where
    get = parseEndianMin FP.anyWord64le 8
instance Put (Endian 'BE Word64) where put = BZ.w64be . unEndian
instance Get (Endian 'BE Word64) where
    get = parseEndianMin FP.anyWord64be 8

instance Put (Endian 'LE Int16) where put = BZ.i16le . unEndian
instance Get (Endian 'LE Int16) where
    get = parseEndianMin FP.anyInt16le 2
instance Put (Endian 'BE Int16) where put = BZ.i16be . unEndian
instance Get (Endian 'BE Int16) where
    get = parseEndianMin FP.anyInt16be 2

instance Put (Endian 'LE Int32) where put = BZ.i32le . unEndian
instance Get (Endian 'LE Int32) where
    get = parseEndianMin FP.anyInt32le 4
instance Put (Endian 'BE Int32) where put = BZ.i32be . unEndian
instance Get (Endian 'BE Int32) where
    get = parseEndianMin FP.anyInt32be 4

instance Put (Endian 'LE Int64) where put = BZ.i64le . unEndian
instance Get (Endian 'LE Int64) where
    get = parseEndianMin FP.anyInt64le 8
instance Put (Endian 'BE Int64) where put = BZ.i64be . unEndian
instance Get (Endian 'BE Int64) where
    get = parseEndianMin FP.anyInt64be 8

-- | Grouping for matching a signedness and size to a Haskell integer data type.
type family IRep (isign :: ISign) (isize :: Natural) where
    IRep 'U 8 = Word8
    IRep 'I 8 =  Int8
    IRep 'U 16 = Word16
    IRep 'I 16 =  Int16
    IRep 'U 32 = Word32
    IRep 'I 32 =  Int32
    IRep 'U 64 = Word64
    IRep 'I 64 =  Int64

-- | Largest representable value for a machine integer made of @n@ bits.
--
-- If signed ''I', twos complement is used, so negative range has 1 extra value.
type family IMax (isign :: ISign) (n :: Natural) :: Natural where
    IMax 'U n = 2^n-1
    IMax 'I n = 2^(n-1)
