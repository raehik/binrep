{- | C-style null-terminated data.

I mix string and bytestring terminology here, due to bad C influences. This
module is specifically interested in bytestrings and their encoding. String/text
encoding is handled in 'Binrep.Type.Text'.
-}

{-# LANGUAGE OverloadedStrings #-} -- for refined errors

module Binrep.Type.NullTerminated where

import Binrep

import FlatParse.Basic qualified as FP

import Rerefined.Predicate.Common
import Rerefined.Refine

import Data.ByteString qualified as B
import Data.Word ( Word8 )

-- | Null-terminated data. Arbitrary length terminated with a null byte.
--   Permits no null bytes inside the data.
data NullTerminate

instance Predicate NullTerminate where
    type PredicateName d NullTerminate = "NullTerminate"

type NullTerminated = Refined NullTerminate

-- | Null-terminated data may not contain any null bytes.
instance Refine NullTerminate B.ByteString where
    -- TODO is there a faster check we can conjure up here...?
    validate p a = validateBool p (not (B.any (== 0x00) a)) $
        "null byte not permitted in null-terminated data"

instance BLen a => BLen (NullTerminated a) where
    blen ra = 1 + blen (unrefine ra)
    {-# INLINE blen #-}

-- | Serialization of null-terminated data may be defined generally using the
--   data's underlying serializer.
instance Put a => Put (NullTerminated a) where
    {-# INLINE put #-}
    put a = put (unrefine a) <> put @Word8 0x00

-- | We may parse any null-terminated data using a special flatparse combinator.
--
-- The combinator doesn't permit distinguishing between the two possible
-- failures: either there was no next null, or the inner parser didn't consume
-- up to it.
instance Get a => Get (NullTerminated a) where
    {-# INLINE get #-}
    get = unsafeRefine <$> cut1 (FP.isolateToNextNull get) e
      where e = [ "while isolating to next null"
                , "either there was no next null in the input,"
                , "or the inner parser didn't fully consume its input" ]

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
