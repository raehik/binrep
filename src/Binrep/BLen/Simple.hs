{-# LANGUAGE UndecidableInstances #-} -- for 'CBLenly', 'TypeError'
{-# LANGUAGE AllowAmbiguousTypes #-} -- for 'cblen', 'natValInt'

module Binrep.BLen.Simple where

import Binrep.CBLen
import GHC.TypeNats
import Util.TypeNats ( natValInt )

import Binrep.Util.Class
import GHC.TypeLits ( TypeError )

import Data.Void
import Data.ByteString qualified as B
import Data.Word
import Data.Int
import Bytezap ( Write(..) )

class BLen a where blen :: a -> Int

instance TypeError ENoEmpty => BLen Void where blen = undefined
instance TypeError ENoSum => BLen (Either a b) where blen = undefined

instance BLen Write where
    {-# INLINE blen #-}
    blen = writeSize

-- | Unit type has length 0.
instance BLen () where
    {-# INLINE blen #-}
    blen () = 0

-- | Sum tuples.
instance (BLen l, BLen r) => BLen (l, r) where
    {-# INLINE blen #-}
    blen (l, r) = blen l + blen r

-- | _O(n)_ Sum the length of each element of a list.
instance BLen a => BLen [a] where
    {-# INLINE blen #-}
    blen = sum . map blen

-- | Length of a bytestring is fairly obvious.
instance BLen B.ByteString where
    {-# INLINE blen #-}
    blen = B.length

-- Machine integers have a constant byte length.
deriving via CBLenly Word8  instance BLen Word8
deriving via CBLenly  Int8  instance BLen  Int8
deriving via CBLenly Word16 instance BLen Word16
deriving via CBLenly  Int16 instance BLen  Int16
deriving via CBLenly Word32 instance BLen Word32
deriving via CBLenly  Int32 instance BLen  Int32
deriving via CBLenly Word64 instance BLen Word64
deriving via CBLenly  Int64 instance BLen  Int64

--------------------------------------------------------------------------------

-- | Deriving via wrapper for types which may derive a 'BLen' instance through
--   an existing 'IsCBLen' instance.
--
-- Examples of such types include machine integers, and explicitly-sized types
-- (e.g. "Binrep.Type.Sized").
newtype CBLenly a = CBLenly { unCBLenly :: a }
instance KnownNat (CBLen a) => BLen (CBLenly a) where
    {-# INLINE blen #-}
    blen _ = cblen @a

-- | Reify a type's constant byte length to the term level.
cblen :: forall a n. (n ~ CBLen a, KnownNat n) => Int
cblen = natValInt @n
{-# INLINE cblen #-}
