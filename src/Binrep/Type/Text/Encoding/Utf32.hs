module Binrep.Type.Text.Encoding.Utf32 where

import Binrep.Type.Text.Internal
import Binrep.Util.ByteOrder

import Refined
import Data.Typeable ( Typeable )

import Data.Text.Encoding qualified as Text
import Data.Text ( Text )

data Utf32 (end :: ByteOrder)

instance Encode (Utf32 BE) where encode' = Text.encodeUtf32BE
instance Encode (Utf32 LE) where encode' = Text.encodeUtf32LE

instance Decode (Utf32 BE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf32BE
instance Decode (Utf32 LE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf32LE

-- | Any 'Text' value is always valid UTF-32.
instance Typeable end => Predicate (Utf32 end) Text where validate _ _ = success
