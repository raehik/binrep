{- | C-style null-terminated data.

I mix string and bytestring terminology here, due to bad C influences. This
module is specifically interested in bytestrings and their encoding. String/text
encoding is handled in 'Binrep.Type.Text'.
-}

{-# LANGUAGE OverloadedStrings #-} -- for refined errors

module Binrep.Type.NullTerminated where

import Binrep

import Refined
import Refined.Unsafe
import Data.Typeable ( typeRep )

import Data.ByteString qualified as B
import Data.Word ( Word8 )

-- | Null-terminated data. Arbitrary length terminated with a null byte.
--   Permits no null bytes inside the data.
data NullTerminate
type NullTerminated = Refined NullTerminate

-- | Null-terminated data may not contain any null bytes.
instance NullCheck a => Predicate NullTerminate a where
    validate p a
     | hasNoNulls a = throwRefineOtherException (typeRep p) $
        "null byte not permitted in null-terminated data"
     | otherwise = success

class NullCheck a where hasNoNulls :: a -> Bool
instance NullCheck B.ByteString where
    {-# INLINE hasNoNulls #-}
    hasNoNulls = B.any (== 0x00)

instance BLen a => BLen (NullTerminated a) where
    blen ra = 1 + blen (unrefine ra)
    {-# INLINE blen #-}

-- | Serialization of null-terminated data may be defined generally using the
--   data's underlying serializer.
instance Put a => Put (NullTerminated a) where
    {-# INLINE put #-}
    put a = put (unrefine a) <> put @Word8 0x00

-- | Parse a null-terminated bytestring.
instance Get (NullTerminated B.ByteString) where
    {-# INLINE get #-}
    get = undefined

{-
I don't know how to do @[a]@. Either I nullterm each element, which is weird
because it's not required in all cases, or I don't, in which case the general
Put doesn't work. Nullterming every element feels weird anyway -- what about
[Word8]?

instance NullCheck a => NullCheck [a] where
    {-# INLINE hasNoNulls #-}
    hasNoNulls = all hasNoNulls
instance NullCheck Word8 where
    {-# INLINE hasNoNulls #-}
    hasNoNulls = \case 0x00 -> False
                       _    -> True
-}
