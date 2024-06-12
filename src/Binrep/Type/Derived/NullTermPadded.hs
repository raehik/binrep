{- | Null-terminated, then null-padded data.

This is defined using the composition of existing 'NullTerminate' and
'NullPad' predicates, plus the re-associating binrep instances for the 'And'
predicate combinator. It kind of just magically works.
-}

module Binrep.Type.Derived.NullTermPadded where

import Binrep.Type.NullTerminated
import Binrep.Type.NullPadded

import Rerefined.Predicate.Logical.And
import Rerefined.Refine ( Refined )

-- | Predicate for null-terminated, then null-padded data.
type NullTermPad n = NullTerminate `And` NullPad n

-- | Null-terminated data, which is then null-padded to the given length.
--
-- Instantiate with @ByteString@ for a null-padded C string.
type NullTermPadded n = Refined (NullTermPad n)
