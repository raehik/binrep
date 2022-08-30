{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.Text.Encoding.Ascii where

import Binrep.Type.Text.Internal
import Binrep.Type.Text.Encoding.Utf8

import Refined
import Data.Typeable ( typeRep )

import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Text ( Text )

-- | 7-bit
data Ascii

-- | We reuse UTF-8 encoding for ASCII, since it is a subset of UTF-8.
instance Encode Ascii where encode' = encode' @Utf8

-- Pre-@text-2.0@, @decodeASCII@ generated a warning and ran @decodeUtf8@.
-- TODO can I give some compile time warning about this instance missing on
-- below text-2.0?? would be cool
#if MIN_VERSION_text(2,0,0)
instance Decode Ascii where decode = decodeText $ wrapUnsafeDecoder Text.decodeASCII
#endif

-- | 'Text' must be validated if you want to permit 7-bit ASCII only.
instance Predicate Ascii Text where
    validate p t = if   Text.all Char.isAscii t
                   then success
                   else throwRefineOtherException (typeRep p) "not valid 7-bit ASCII"
