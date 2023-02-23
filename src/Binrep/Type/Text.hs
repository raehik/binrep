{- TODO 2023-02-15 raehik
Encoding shouldn't change to a bytestring, for efficiency. Due to convenient
representations, we can efficiently serialize a Text directly to a builder,
skipping intermediate ByteString conversion.

Hm. Maybe that means it should be changed to the builder. What does that mean
for decoding?
-}

module Binrep.Type.Text
  ( AsText
  , Encode(..), encode, encodeToRep
  , Decode(..)

  , module Binrep.Type.Text.Encoding.Utf8
  , module Binrep.Type.Text.Encoding.Ascii
  , module Binrep.Type.Text.Encoding.Utf16
  , module Binrep.Type.Text.Encoding.Utf32
  , module Binrep.Type.Text.Encoding.ShiftJis

  ) where

import Binrep.Type.Text.Internal

import Refined

import Binrep.Type.Text.Encoding.Utf8
import Binrep.Type.Text.Encoding.Ascii
import Binrep.Type.Text.Encoding.Utf16
import Binrep.Type.Text.Encoding.Utf32
import Binrep.Type.Text.Encoding.ShiftJis

-- | Encode some validated text.
encode :: forall enc. Encode enc => AsText enc -> Bytes
encode = encode' @enc . unrefine

-- | Encode some text to a bytestring, asserting that the resulting value is
--   valid for the requested bytestring representation.
--
-- This is intended to be used with visible type applications:
--
-- >>> let Right t = refine @UTF8 (Text.pack "hi")
-- >>> :t t
-- t :: AsText UTF8
-- >>> let Right bs = encodeToRep @'C t
-- >>> :t bs
-- bs :: Refined 'C Bytes
encodeToRep
    :: forall rep enc
    .  (Encode enc, ApplyPred rep Bytes)
    => AsText enc
    -> Either RefineException (Refined rep Bytes)
encodeToRep = refine . encode
