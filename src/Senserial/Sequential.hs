{- | Generic sequential parsing and serializaton.

By sequential, we mean that individual constructors are handled by concatenating
consecutive fields. (The sort of design often seen in low-level data formats.)

Sum types are handled by prefixing a tag used to indicate the constructor that
follows. The user selects the prefix tag type to use. The prefix tag uses the
same class as the base cases, so ensure it has the relevant instance.

The parsers and serializers are split into sum type handlers (>1 constructor)
and non-sum type handlers (1 constructor). The non-sum type handler skips the
prefix tag work. An invalid configuration (e.g. using a sum type handler with a
non-sum type) will result in a descriptive type error. This is a design choice
to assist in catching errors.

Furthermore, neither handler type permits empty datatypes. Incorrect usage will
result in a descriptive type error.

Generated instances should be fairly performant. Due to the design of 'Generic',
instances for sum types will inevitably have worse performance than hand-written
instances. If you want the best performance possible, write your own instance.

Note your type's 'Generic' instance _must_ be decorated with metadata. So only
use these with GHC's automatically derived 'Generic' instances.
-}

module Senserial.Sequential
  ( module Senserial.Sequential.Serialize
  , module Senserial.Sequential.Parse
  ) where

import Senserial.Sequential.Serialize
import Senserial.Sequential.Parse
