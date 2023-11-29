{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Predicates and serialization for character encodings.

This is somewhat unrelated to binrep, but due to overlaps like Haskell's 'Text'
type using UTF-8, we include it here.

As of text-2.1 (? ish, when someone redid internal encoding with streaming), we
could implement some faster serialization by hooking directly into the
streaming. So we don't have to build a bytestring and copy-- we can build
in-place instead.

In-place parsing is less obvious and probably needs more bespoke code, except
maybe for UTF-8. Also, I think I have to do @takeRest@ for every one, which
feels bad (probably why I didn't do it before).
-}

module Binrep.Type.Text where

import Binrep

import Refined

import Bytezap.Text qualified as BZ

import Data.Text ( Text )
import Data.Text.Encoding qualified as Text
import Data.ByteString qualified as B

import Binrep.Type.Common ( Endianness(..) )

import Data.Typeable ( Typeable, typeRep )

#ifdef HAVE_ICU
import Data.Text.ICU.Convert qualified as ICU
import System.IO.Unsafe qualified
#endif

data Utf8

-- | Any 'Text' value is always valid UTF-8.
instance Predicate Utf8 Text where validate _ _ = success

-- | A 'ByteString' may be validated as UTF-8 text.
instance Predicate Utf8 B.ByteString where
    validate p bs
      | B.isValidUtf8 bs = success
      | otherwise        =
            throwRefineOtherException (typeRep p) "bytestring not valid UTF-8"

instance Put (Refined Utf8 Text) where
    put = BZ.textUtf8 . unrefine

data Utf16 (end :: Endianness)

-- | Any 'Text' value is always valid UTF-16.
instance Typeable end => Predicate (Utf16 end) Text where validate _ _ = success

-- | Serialize UTF-16LE through a bytestring (inefficient).
instance Put (Refined (Utf16 'BE) Text) where
    put = put . Text.encodeUtf16BE . unrefine

-- No fast UTF-16 serializer in bytezap yet.

data Utf32 (end :: Endianness)

-- | Any 'Text' value is always valid UTF-32.
instance Typeable end => Predicate (Utf32 end) Text where validate _ _ = success

-- No fast UTF-32 serializer in bytezap yet.

data ShiftJis

-- | TODO Unsafely assume all 'Text's are valid Shift-JIS.
--
-- Should be true in 99% of cases, but almost certainly not 100%.
instance Predicate ShiftJis Text where validate _ _ = success

-- TODO re-add encoding via bytestring

#ifdef HAVE_ICU
-- | Encode some 'Text' to the given character set using text-icu.
--
-- No guarantees about correctness. Encodings are weird. e.g. Shift JIS's
-- yen/backslash problem is apparently to do with OSs treating it differently.
--
-- Expects a 'Text' that is confirmed valid for converting to the character set.
--
-- The charset must be valid, or it's exception time. See text-icu.
encodeViaTextICU :: String -> Text -> IO B.ByteString
encodeViaTextICU charset t = do
    conv <- ICU.open charset Nothing
    pure $ ICU.fromUnicode conv t

encodeViaTextICU' :: String -> Text -> B.ByteString
encodeViaTextICU' charset t =
    System.IO.Unsafe.unsafeDupablePerformIO $ encodeViaTextICU charset t

-- TODO Shitty library doesn't let us say how to handle errors. Apparently, the
-- only solution is to scan through the resulting 'Text' to look for @\SUB@
-- characters, or lie about correctness. Sigh.
decodeViaTextICU :: String -> B.ByteString -> IO (Either String Text)
decodeViaTextICU charset t = do
    conv <- ICU.open charset Nothing
    pure $ Right $ ICU.toUnicode conv t

decodeViaTextICU' :: String -> B.ByteString -> Either String Text
decodeViaTextICU' charset t = do
    System.IO.Unsafe.unsafeDupablePerformIO $ decodeViaTextICU charset t
#endif
