{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Binrep.Type.Text
  ( Encoding(..)
  , AsText
  , Encode, encode
  , Decode(..)
  , encodeToRep
#ifdef HAVE_ICU
  , decodeViaTextICU
#endif
  ) where

import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.ByteString ( Rep )

import Refined
import Refined.Unsafe

import Data.ByteString qualified as B
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Char qualified as Char
import Data.Text.Encoding qualified as Text
import Data.Either.Combinators qualified as Either

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Typeable ( Typeable, typeRep )

import System.IO.Unsafe qualified
import Control.Exception qualified
import Data.Text.Encoding.Error qualified

#ifdef HAVE_ICU
import Data.Text.ICU.Convert qualified as ICU
#endif

type Bytes = B.ByteString

-- | Character encoding.
--
-- Byte-oriented encodings like ASCII and UTF-8 don't need to worry about
-- endianness. For UTF-16 and UTF-32, the designers decided to allow different
-- endiannesses, rather than saying "codepoints must be X-endian".
data Encoding
  = UTF8
  | UTF16 Endianness
  | UTF32 Endianness
  | ASCII -- ^ 7-bit
  | SJIS
    deriving stock (Generic, Data, Show, Eq)

-- | A string of a given encoding, stored in the 'Text' type.
type AsText (enc :: Encoding) = Refined enc Text

-- | Bytestring encoders for text validated for a given encoding.
class Encode (enc :: Encoding) where
    -- | Encode text to bytes. Internal function, use 'encode'.
    encode' :: Text -> Bytes

instance Encode 'UTF8 where encode' = Text.encodeUtf8

-- | ASCII is a subset of UTF-8, so valid ASCII is valid UTF-8, so this is safe.
instance Encode 'ASCII where encode' = encode' @'UTF8

instance Encode ('UTF16 'BE) where encode' = Text.encodeUtf16BE
instance Encode ('UTF16 'LE) where encode' = Text.encodeUtf16LE
instance Encode ('UTF32 'BE) where encode' = Text.encodeUtf32BE
instance Encode ('UTF32 'LE) where encode' = Text.encodeUtf32LE

-- | Encode some validated text.
encode :: forall enc. Encode enc => AsText enc -> Bytes
encode = encode' @enc . unrefine

-- | Any 'Text' value is always valid UTF-8.
instance Predicate 'UTF8 Text where validate _ _ = success

-- | Any 'Text' value is always valid UTF-16.
instance Typeable e => Predicate ('UTF16 e) Text where validate _ _ = success

-- | Any 'Text' value is always valid UTF-32.
instance Typeable e => Predicate ('UTF32 e) Text where validate _ _ = success

-- | 'Text' must be validated if you want to permit 7-bit ASCII only.
instance Predicate 'ASCII Text where
    validate p t = if   Text.all Char.isAscii t
                   then success
                   else throwRefineOtherException (typeRep p) "not valid 7-bit ASCII"

-- | TODO Unsafely assume all 'Text's are valid Shift-JIS.
instance Predicate 'SJIS Text where validate _ _ = success

class Decode (enc :: Encoding) where
    -- | Decode a 'ByteString' to 'Text' with an explicit encoding.
    --
    -- This is intended to be used with visible type applications.
    decode :: Bytes -> Either String (AsText enc)

instance Decode 'UTF8  where decode = decodeText show Text.decodeUtf8'
instance Decode ('UTF16 'BE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf16BE
instance Decode ('UTF16 'LE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf16LE
instance Decode ('UTF32 'BE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf32BE
instance Decode ('UTF32 'LE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf32LE

-- Pre-@text-2.0@, @decodeASCII@ generated a warning and ran @decodeUtf8@.
#if MIN_VERSION_text(2,0,0)
instance Decode 'ASCII where decode = decodeText $ wrapUnsafeDecoder Text.decodeASCII
#endif

--------------------------------------------------------------------------------
-- Helpers

-- | Encode some text to a bytestring, asserting that the resulting value is
--   valid for the requested bytestring representation.
--
-- This is intended to be used with visible type applications:
--
-- >>> let Right t = refine @'UTF8 (Text.pack "hi")
-- >>> :t t
-- t :: AsText 'UTF8
-- >>> let Right bs = encodeToRep @'C t
-- >>> :t bs
-- bs :: Refined 'C Bytes
encodeToRep
    :: forall (rep :: Rep) enc
    .  (Encode enc, Predicate rep Bytes)
    => AsText enc
    -> Either RefineException (Refined rep Bytes)
encodeToRep = refine . encode

--------------------------------------------------------------------------------
-- Internal helpers

-- | Helper for decoding a 'Bytes' to a 'Text' tagged with its encoding.
decodeText
    :: forall enc e
    .  (e -> String) -> (Bytes -> Either e Text) -> Bytes
    -> Either String (AsText enc)
decodeText g f = Either.mapBoth g reallyUnsafeRefine . f

-- | Run an unsafe decoder safely.
--
-- Copied from @Data.Text.Encoding.decodeUtf8'@, so should be bulletproof?
wrapUnsafeDecoder
    :: (Bytes -> Text)
    -> Bytes -> Either Data.Text.Encoding.Error.UnicodeException Text
wrapUnsafeDecoder f =
      System.IO.Unsafe.unsafeDupablePerformIO
    . Control.Exception.try
    . Control.Exception.evaluate
    . f

--------------------------------------------------------------------------------
-- ICU

#ifdef HAVE_ICU
instance Encode 'SJIS where encode' = encodeViaTextICU' "Shift-JIS"
instance Decode 'SJIS where
    decode  = decodeText id $ decodeViaTextICU' "Shift-JIS"

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
    return $ ICU.fromUnicode conv t

encodeViaTextICU' :: String -> Text -> B.ByteString
encodeViaTextICU' charset t =
    System.IO.Unsafe.unsafeDupablePerformIO $ encodeViaTextICU charset t

-- TODO Shitty library doesn't let us say how to handle errors. Apparently, the
-- only solution is to scan through the resulting 'Text' to look for @\SUB@
-- characters, or lie about correctness. Sigh.
decodeViaTextICU :: String -> B.ByteString -> IO (Either String Text)
decodeViaTextICU charset t = do
    conv <- ICU.open charset Nothing
    return $ Right $ ICU.toUnicode conv t

decodeViaTextICU' :: String -> B.ByteString -> Either String Text
decodeViaTextICU' charset t = do
    System.IO.Unsafe.unsafeDupablePerformIO $ decodeViaTextICU charset t
#endif
