{- | C-style null-terminated data.

I mix string and bytestring terminology here, due to bad C influences. This
module is specifically interested in bytestrings and their encoding. String/text
encoding is handled in 'Binrep.Type.Text'.
-}

{-# LANGUAGE OverloadedStrings #-} -- for refined errors

module Binrep.Type.NullTerminated where

import Binrep

import FlatParse.Basic qualified as FP

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

-- | We get efficient general serialization of null-terminated data
--   using the data's underlying serializer.
instance Put a => Put (NullTerminated a) where
    {-# INLINE put #-}
    put a = put (unrefine a) <> put @Word8 0x00

{- TODO

We may generally parse null-terminated data by checking ahead for a null,
isolating and using the data's underlying parser.

I thought this might be inefficient. But actually, it might be fine. The base
case, bytestrings, works like that in flatparse. We just need a new combinator.

2023-11-30T10:29:37+0000 https://github.com/AndrasKovacs/flatparse/pull/50/files
yep, nice and easy :)

instance Get a => Get (NullTerminated a) where
    {-# INLINE get #-}
    -- the error wrapping is gonna look ugly idk
    get = reallyUnsafeRefine <$> getEBase (isolateToNextNull (getEBase get) ...)
-}

-- | Parse a null-terminated bytestring.
--
-- TODO almost certainly could improve error wrapping.
instance Get a => Get (NullTerminated a) where
    {-# INLINE get #-}
    get = reallyUnsafeRefine <$> p
      where p = getEBase (fpIsolateToNextNull get) (EFailNamed "null-terminated data")

-- TODO 2023-12-03: waiting on a flatparse release
fpIsolateToNextNull :: Getter a -> Getter a

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
