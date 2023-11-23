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

import Data.Text.Encoding qualified as Text
import Control.Exception qualified
import System.IO.Unsafe qualified

-- | 7-bit
data Ascii

-- | We reuse UTF-8 encoding for ASCII, since it is a subset of UTF-8.
instance Encode Ascii where encode' = encode' @Utf8

-- Pre-@text-2.0@, @decodeASCII@ generated a warning and ran @decodeUtf8@.
-- TODO can I give some compile time warning about this instance missing on
-- below text-2.0?? would be cool
#if MIN_VERSION_text(2,0,0)
-- TODO 2023-01-26 raehik: awful UX by text. hopefully safe lol?? works at least
instance Decode Ascii where decode = decodeText id $ catchErrorCall Text.decodeASCII
#endif

catchErrorCall :: (a -> b) -> a -> Either String b
catchErrorCall f a = System.IO.Unsafe.unsafeDupablePerformIO $ do
    Control.Exception.try @Control.Exception.ErrorCall (Control.Exception.evaluate (f a)) >>= \case
      Right b -> pure $ Right b
      Left  (Control.Exception.ErrorCallWithLocation msg _) -> pure $ Left msg

-- | 'Text' must be validated if you want to permit 7-bit ASCII only.
--
-- TODO there should be a MUCH faster check here in text-2.0. text-short has it,
-- text doesn't yet. see: https://github.com/haskell/text/issues/496
instance Predicate Ascii Text where
    validate p t = if   Text.all Char.isAscii t
                   then success
                   else throwRefineOtherException (typeRep p) "not valid 7-bit ASCII"
