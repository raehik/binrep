module Binrep.Type.Text.Encoding.Utf8 where

import Binrep.Type.Text.Internal

import Refined

import Data.Text.Encoding qualified as Text
import Data.Text ( Text )

data Utf8
instance Pred Utf8

instance Encode Utf8 where encode' = Text.encodeUtf8
instance Decode Utf8 where decode  = decodeText show Text.decodeUtf8'

-- | Any 'Text' value is always valid UTF-8.
instance ApplyPred Utf8 Text where validate _ _ = success
