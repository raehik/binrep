{- | Generic sequential parsing.

We gather data type metadata during traversal and ferry it down, to then reflect
as needed in the "lower" parsers. For example, the @C1@ constructor handler
takes the constructor metadata type and ferries it down to the @S1@ record
handler, which uses it to fill out parse errors with detailed information.
-}

module Senserial.Sequential.Parse
  ( SeqParser(..)
  , seqParseSum, type SeqParseSum, SeqParserSum(..), PfxTagCfg(..)
  , seqParseNonSum, type SeqParseNonSum
  ) where

import Senserial.Sequential.Parse.Parser
import Senserial.Sequential.Parse.Sum
import Senserial.Sequential.Parse.NonSum
