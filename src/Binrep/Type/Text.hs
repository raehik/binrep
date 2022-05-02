{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

module Binrep.Type.Text
  ( Encoding(..)
  , AsText
  , Encode, encode
  , Decode(..)
  , encodeToRep
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
import Data.Typeable ( typeRep )
import Data.Either.Combinators qualified as Either

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

import System.IO.Unsafe qualified
import Control.Exception qualified
import Data.Text.Encoding.Error qualified

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
    deriving stock (Generic, Typeable, Data, Show, Eq)

-- | A string of a given encoding, stored in the 'Text' type.
type AsText (enc :: Encoding) = Refined enc Text

-- | Bytestring encoders for text validated for a given encoding.
class Encode (enc :: Encoding) where
    -- | Encode text to bytes. Internal function, use 'encode'.
    encode' :: Text -> Bytes

instance Encode 'UTF8 where encode' = Text.encodeUtf8

-- | ASCII is a subset of UTF-8, so valid ASCII is valid UTF-8, thus this is
--   safe. Though if there was a special ASCII-only text type, we could use it
--   to be really efficient (I guess that's just bytestrings lol). Also would
--   way overcomplicate this setup for a smidge of performance x)
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

-- | 'Text' must be validated if you want to permit ASCII only.
instance Predicate 'ASCII Text where
    validate p t = if   Text.all Char.isAscii t
                   then success
                   else throwRefineOtherException (typeRep p) "non-ASCII text"

class Decode (enc :: Encoding) where
    -- | Decode a 'ByteString' to 'Text' with an explicit encoding.
    --
    -- This is intended to be used with visible type applications.
    decode :: Bytes -> Either String (AsText enc)

instance Decode 'UTF8  where decode = decodeText Text.decodeUtf8'
instance Decode ('UTF16 'BE) where decode = decodeText $ wrapUnsafeDecoder Text.decodeUtf16BE
instance Decode ('UTF16 'LE) where decode = decodeText $ wrapUnsafeDecoder Text.decodeUtf16LE
instance Decode ('UTF32 'BE) where decode = decodeText $ wrapUnsafeDecoder Text.decodeUtf32BE
instance Decode ('UTF32 'LE) where decode = decodeText $ wrapUnsafeDecoder Text.decodeUtf32LE

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
    :: forall enc e. Show e
    => (Bytes -> Either e Text) -> Bytes
    -> Either String (AsText enc)
decodeText f = Either.mapBoth show reallyUnsafeRefine . f

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
