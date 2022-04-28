-- | Derive 'BLen', 'CBLen', 'Put' and 'Get' instances generically.

module Binrep.Generic
  ( Binrep.Generic.Internal.Cfg(..)
  , cSumTagHex
  , blenGeneric, putGeneric, getGeneric, CBLenGeneric
  ) where

import Binrep.Generic.Internal qualified
import Binrep.Generic.BLen
import Binrep.Generic.Put
import Binrep.Generic.Get
import Binrep.Generic.CBLen

import Numeric ( readHex )

-- | Obtain the tag for a sum type value by applying a function to the
--   constructor name, and reading the result as a hexadecimal number.
cSumTagHex :: Integral a => (String -> String) -> String -> a
cSumTagHex f = forceRead . readHex . f

-- | Successfully parse exactly one result, or runtime error.
forceRead :: [(a, String)] -> a
forceRead = \case []        -> error "no parse"
                  [(x, "")] -> x
                  [(_x, _)] -> error "incomplete parse"
                  (_:_)     -> error "too many parses (how??)"
