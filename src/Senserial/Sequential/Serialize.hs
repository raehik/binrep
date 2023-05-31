-- | Generic sequential serialization.

module Senserial.Sequential.Serialize
  ( SeqBuilder(..)
  , seqSerSum, type SeqSerSum
  , seqSerNonSum, type SeqSerNonSum
  ) where

import Senserial.Sequential.Serialize.Builder
import Senserial.Sequential.Serialize.Sum
import Senserial.Sequential.Serialize.NonSum
