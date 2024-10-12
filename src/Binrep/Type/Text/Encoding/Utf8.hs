module Binrep.Type.Text.Encoding.Utf8 where

import Binrep.Type.Text.Internal

import Rerefined.Predicate

import Data.Text.Encoding qualified as Text
import Data.Text ( Text )

data Utf8
instance Predicate Utf8 where type PredicateName d Utf8 = "UTF-8"

-- | Any 'Text' value is always valid UTF-8.
instance Refine Utf8 Text where validate _ _ = Nothing

instance Encode Utf8 where encode' = Text.encodeUtf8
instance Decode Utf8 where decode  = decodeText show Text.decodeUtf8'
