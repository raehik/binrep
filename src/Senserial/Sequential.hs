{- Generic sequential serialization.

In the name of explicitness, there are two generic serializers:

  * a non-sum type serializer, which only permits data types containing a single
    constructor
  * a sum type serializer, which only permits data types containing >1
    constructors

Neither serializer permits empty datatypes. Incorrect usage will result in a
descriptive type error.

Generated instances should be fairly performant. Due to the design of 'Generic',
instances for sum types will inevitably have worse performance than hand-written
instances. If you want the best performance possible, write your own instance.

Note your type's 'Generic' instance _must_ be decorated with metadata. So only
use these with GHC's automatically derived 'Generic' instances.
-}

module Senserial.Sequential
  ( type Senserial.Sequential.Sum.SeqSerSum
  ,      Senserial.Sequential.Sum.seqSerSum
  , type Senserial.Sequential.NonSum.SeqSerNonSum
  ,      Senserial.Sequential.NonSum.seqSerNonSum
  ) where

import Senserial.Sequential.Sum qualified
import Senserial.Sequential.NonSum qualified
