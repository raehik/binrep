{- TODO 2023-11-22 raehik
This is a convenience layer. We could do these directly on Put and Get instead,
and get better efficiency for encoding conversions which have efficient
implementations in our builder (UTF8 is the big one). In fact, that might be
best. The way this is coded, this is a non-binrep feature that we're wrapping
into binrep.
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
    .  (Encode enc, Predicate rep Bytes)
    => AsText enc
    -> Either RefineException (Refined rep Bytes)
encodeToRep = refine . encode
