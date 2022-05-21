{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.BLen
  ( module Binrep.BLen
  , module Binrep.BLen.Internal.AsBLen
  ) where

import Binrep.BLen.Internal.AsBLen
import Binrep.CBLen
import Binrep.Util ( natVal'' )

import GHC.TypeNats
import Data.ByteString qualified as B
import Data.Word
import Data.Int

type BLenT = Int

-- | The length in bytes of a value of the given type can be known on the cheap
--   e.g. by reading a length field, or using compile time information.
--
-- Concepts such as null padding require the notion of length in bytes in order
-- to handle. In a hand-rolled parser, you may keep count of the current length
-- as you go. Here, the individual types keep track, and expose it via this
-- typeclass.
--
-- Obtaining the length of a value is usually an @O(1)@ operation like reading a
-- field or returning a constant. When it's not, it's often an indicator of a
-- problematic type e.g. plain Haskell lists.
--
-- We derive a default instance for constant-size types by throwing away the
-- value and reifying the type level natural.
--
-- Note that one can derive a free 'BLen' instance for any type with a 'Put'
-- instance via serializing it and checking the length. _Do not do this._ If you
-- find you can't write a decent 'BLen' instance for a type, it may be that you
-- need to rethink the representation.
class BLen a where
    blen :: a -> BLenT
    default blen :: KnownNat (CBLen a) => a -> BLenT
    blen _ = cblen @a

typeNatToBLen :: forall n. KnownNat n => BLenT
typeNatToBLen = natToBLen $ natVal'' @n
{-# INLINE typeNatToBLen #-}

-- | Reify a type's constant byte length to the term level.
cblen :: forall a n. (n ~ CBLen a, KnownNat n) => BLenT
cblen = typeNatToBLen @n
{-# INLINE cblen #-}

-- | @O(n)@
instance BLen a => BLen [a] where
    blen = sum . map blen

instance (BLen a, BLen b) => BLen (a, b) where
    blen (a, b) = blen a + blen b

instance BLen B.ByteString where
    blen = posIntToBLen . B.length

deriving anyclass instance BLen Word8
deriving anyclass instance BLen  Int8
deriving anyclass instance BLen Word16
deriving anyclass instance BLen  Int16
deriving anyclass instance BLen Word32
deriving anyclass instance BLen  Int32
deriving anyclass instance BLen Word64
deriving anyclass instance BLen  Int64
