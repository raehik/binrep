module Binrep.BLen where

import Binrep.CBLen
import Binrep.Util

import GHC.TypeNats
import Data.ByteString qualified as B
import Data.Word
import Data.Int

-- | The length in bytes of a value of the given type can be known, preferably
--   on the cheap e.g. reading a length field, or statically at compile time.
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
class BLen a where
    blen :: a -> Natural
    default blen :: KnownNat (CBLen a) => a -> Natural
    blen _ = cblen @a

-- | @O(n)@
instance BLen a => BLen [a] where
    blen = sum . map blen

instance (BLen a, BLen b) => BLen (a, b) where
    blen (a, b) = blen a + blen b

instance BLen B.ByteString where
    blen = unsafePosIntToNat . B.length

deriving anyclass instance BLen Word8
deriving anyclass instance BLen  Int8
deriving anyclass instance BLen Word16
deriving anyclass instance BLen  Int16
deriving anyclass instance BLen Word32
deriving anyclass instance BLen  Int32
deriving anyclass instance BLen Word64
deriving anyclass instance BLen  Int64
