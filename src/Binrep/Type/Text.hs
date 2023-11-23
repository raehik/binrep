module Binrep.Type.Text where

import Binrep

import Refined

import Bytezap.Text qualified as BZ

import Data.Text ( Text )

-- | Validated @enc@ text stored in an @a@.
--
-- TODO should this be a newtype instead? idk
type TextEncodingAs enc a = Refined enc a

data Utf8

-- | Any 'Text' value is always valid UTF-8.
instance Predicate Utf8 Text where validate _ _ = success

instance Put (Refined Utf8 Text) where
    put = BZ.textUtf8 . unrefine

{-
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
-}
