{- TODO
Potentially should be moved into BinaryCodec. We need this for padding-based
combinators. Each inside combinator must report the number of bytes they
serialized. It *should* be extremely cheap.

So far I haven't got another use for it, and I feel there should be some better
way to define it but the types don't fit well. So I'll leave it as is for now.
-}

module Binscheme.ByteLen where

import Numeric.Natural

-- | The size in bytes of the type can be known, preferably on the cheap e.g.
--   reading a length field.
--
-- Aim to make this O(1). Except for where you can't like lists lol.
--
-- Importantly goes hand in hand with BinaryCodec! Maybe we should make it a
-- function there lol!
--
-- This is useful for padding types generically, without having to track the
-- read/write cursor. Yes, that feature *is* common and cheap for parsers (and
-- to lesser extent, serializers), but you should really only be padding types
-- that have a "static-ish" (= cheaply calculated) size. At least, I think so?
-- I'm not quite sure.
--
-- Some instances ignore the argument. It would be possible to pull those out
-- into a statically-known bytelength typeclass, but it wouldn't improve clarity
-- or performance, just get rid of a couple 'undefined's.
class ByteLen a where
    blen :: a -> Natural

instance ByteLen a => ByteLen [a] where blen = sum . map blen
