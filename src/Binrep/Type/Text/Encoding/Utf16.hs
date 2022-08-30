module Binrep.Type.Text.Encoding.Utf16 where

import Binrep.Type.Text.Internal
import Binrep.Type.Common ( Endianness(..) )

import Refined
import Data.Typeable ( Typeable )

import Data.Text.Encoding qualified as Text
import Data.Text ( Text )

data Utf16 (end :: Endianness)

instance Encode (Utf16 'BE) where encode' = Text.encodeUtf16BE
instance Encode (Utf16 'LE) where encode' = Text.encodeUtf16LE

instance Decode (Utf16 'BE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf16BE
instance Decode (Utf16 'LE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf16LE

-- | Any 'Text' value is always valid UTF-16.
instance (Typeable end) => Predicate (Utf16 end) Text where validate _ _ = success
