{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- for CBLen

module Binrep.BLen
  ( module Binrep.BLen
  , module Binrep.BLen.Internal.AsBLen
  ) where

import Binrep.BLen.Internal.AsBLen
import Binrep.Util ( natVal'' )

import GHC.TypeLits

import Data.ByteString qualified as B

import Data.Word
import Data.Int

import Data.Void ( Void, absurd )

type BLenT = Int

{- | The length in bytes of a value of the given type can be known on the cheap
     e.g. by reading a length field, or using compile time information.

Some binary representation building blocks require the notion of length in bytes
in order to handle, e.g. null padding. One may always obtain this by serializing
the value, then reading out the length of the output bytestring. But in most
cases, we can be much more efficient.

  * Certain primitives have a size known at compile time, irrelevant of the
    value. A 'Word64' is always 8 bytes; some data null-padded to @n@ bytes is
    exactly @n@ bytes long.
  * For simple ADTs, it's often possible to calculate length in bytes via
    pattern matching and some numeric operations. Very little actual work.

This type class enables each type to implement its own efficient method of byte
length calculation. Aim to write something that plainly feels more efficient
than full serialization. If that doesn't feel possible, you might be working
with a type ill-suited for binary representation.

A thought: Some instances could be improved by reifying 'CBLen'. But it would
mess up all the deriving, and it feels like too minor an improvement to be
worthwhile supporting, writing a bunch of newtype wrappers, etc.
-}
class BLen a where
    -- | The length in bytes of the serialized value.
    --
    -- The default implementation reifies the constant length for the type. If a
    -- type-wide constant length is not defined, it will fail at compile time.
    blen :: a -> BLenT
    default blen :: KnownNat (CBLen a) => a -> BLenT
    blen _ = cblen @a

    -- | The length in bytes of any value of the given type is constant.
    --
    -- Many binary representation primitives are constant, or may be designed to
    -- "store" their size in their type. This is a stronger statement about
    -- their length than just 'blen'.
    --
    -- This is now an associated type family of the 'BLen' type class in hopes
    -- of simplifying the binrep framework.
    type CBLen a :: Natural
    type CBLen a =
        TypeError
            (       'Text "No CBLen associated family instance defined for "
              ':<>: 'ShowType a
            )

typeNatToBLen :: forall n. KnownNat n => BLenT
typeNatToBLen = natToBLen $ natVal'' @n
{-# INLINE typeNatToBLen #-}

-- | Reify a type's constant byte length to the term level.
cblen :: forall a n. (n ~ CBLen a, KnownNat n) => BLenT
cblen = typeNatToBLen @n
{-# INLINE cblen #-}

-- | Impossible to put a byte length to 'Void'.
instance BLen Void where
    blen = absurd

-- | @O(n)@
instance BLen a => BLen [a] where
    blen = sum . map blen

instance (BLen a, BLen b) => BLen (a, b) where
    blen (a, b) = blen a + blen b

instance BLen B.ByteString where
    blen = posIntToBLen . B.length

instance BLen Word8  where type CBLen Word8  = 2^0
instance BLen  Int8  where type CBLen  Int8  = 2^0
instance BLen Word16 where type CBLen Word16 = 2^1
instance BLen  Int16 where type CBLen  Int16 = 2^1
instance BLen Word32 where type CBLen Word32 = 2^2
instance BLen  Int32 where type CBLen  Int32 = 2^2
instance BLen Word64 where type CBLen Word64 = 2^3
instance BLen  Int64 where type CBLen  Int64 = 2^3

--------------------------------------------------------------------------------

-- | Newtype wrapper for defining 'BLen' instances which are allowed to assume
--   the existence of a valid 'CBLen' family instance.
newtype WithCBLen a = WithCBLen { unWithCBLen :: a }

instance KnownNat (CBLen a) => BLen (WithCBLen [a]) where
    blen (WithCBLen l) = cblen @a * posIntToBLen (length l)
instance KnownNat (CBLen a + CBLen b) => BLen (WithCBLen (a, b)) where
    type CBLen (WithCBLen (a, b)) = CBLen a + CBLen b
