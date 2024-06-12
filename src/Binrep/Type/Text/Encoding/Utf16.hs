{-# LANGUAGE UndecidableInstances #-} -- for PredicateName

module Binrep.Type.Text.Encoding.Utf16 where

import Binrep.Type.Text.Internal
import Binrep.Util.ByteOrder

import Rerefined.Predicate
import TypeLevelShow.Utils

import Data.Text.Encoding qualified as Text
import Data.Text ( Text )

data Utf16 (end :: ByteOrder)
instance Predicate (Utf16 end) where
    type PredicateName d (Utf16 end) = "UTF-16" ++ EndianSuffix end

instance Encode (Utf16 BE) where encode' = Text.encodeUtf16BE
instance Encode (Utf16 LE) where encode' = Text.encodeUtf16LE

instance Decode (Utf16 BE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf16BE
instance Decode (Utf16 LE) where decode = decodeText show $ wrapUnsafeDecoder Text.decodeUtf16LE

-- | Any 'Text' value is always valid UTF-16.
instance Refine (Utf16 end) Text where validate _ _ = Nothing
