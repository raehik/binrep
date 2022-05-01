-- | Derive 'BLen', 'Put', 'Get' and 'CBLen' instances generically.

module Binrep.Generic
  ( Binrep.Generic.Internal.Cfg(..)
  , cSumTagHex, cSumTagNullTerm
  , blenGeneric, putGeneric, getGeneric, CBLenGeneric
  ) where

import Binrep.Generic.Internal qualified
import Binrep.Generic.BLen
import Binrep.Generic.Put
import Binrep.Generic.Get
import Binrep.Generic.CBLen

import Binrep.Type.ByteString ( AsByteString, Rep(..) )
import Refined.Unsafe ( reallyUnsafeRefine )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Numeric ( readHex )

-- TODO better error handling (see what aeson does)

-- | Obtain the tag for a sum type value by applying a function to the
--   constructor name, and reading the result as a hexadecimal number.
cSumTagHex :: forall a. Integral a => (String -> String) -> String -> a
cSumTagHex f = forceRead . readHex . f

-- | Successfully parse exactly one result, or runtime error.
forceRead :: [(a, String)] -> a
forceRead = \case []        -> error "no parse"
                  [(x, "")] -> x
                  [(_x, _)] -> error "incomplete parse"
                  (_:_)     -> error "too many parses (how??)"

-- | Obtain the tag for a sum type value using the constructor name directly
--   (with a null terminator).
--
-- This is probably not what you want in a binary representation, but it's safe
-- and may be useful for debugging.
--
-- The refine force is safe under the assumption that Haskell constructor names
-- are UTF-8 with no null bytes allowed. I haven't confirmed that, but I'm
-- fairly certain.
cSumTagNullTerm :: String -> AsByteString 'C
cSumTagNullTerm = reallyUnsafeRefine . Text.encodeUtf8 . Text.pack
