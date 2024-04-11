-- | Various types derived from other binrep types.

module Binrep.Type.Derived where

import Binrep.Type.NullTerminated
import Binrep.Type.NullPadded

import Refined

-- | Predicate for null-terminated, then null-padded data.
type NullTermPad n = NullTerminate `And` NullPad n

-- | Null-terminated data, which is then null-padded to the given length.
--
-- Instantiate with @ByteString@ for a null-padded C string.
type NullTermPadded n = Refined (NullTermPad n)
