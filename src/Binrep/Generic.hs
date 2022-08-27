-- | Derive 'BLen', 'Put', 'Get' and 'CBLen' instances generically.

module Binrep.Generic
  ( Cfg(..), cfg
  , cSumTagHex, cSumTagNullTerm, cDef
  , cNoSum, EDerivedSumInstanceWithNonSumCfg(..)
  , blenGeneric, putGeneric, getGeneric, CBLenGeneric
  ) where

import Binrep.Generic.Internal
import Binrep.Generic.BLen
import Binrep.Generic.Put
import Binrep.Generic.Get
import Binrep.Generic.CBLen

import Binrep.Type.ByteString ( AsByteString, Rep(..) )
import Refined.Unsafe ( reallyUnsafeRefine )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Numeric ( readHex )

import Data.Void ( Void )
import Control.Exception ( Exception, throw )

cfg :: Eq a => (String -> a) -> Cfg a
cfg f = Cfg { cSumTag = f, cSumTagEq = (==) }

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

-- | Default generic derivation configuration, using 'cSumTagNullTerm'.
cDef :: Cfg (AsByteString 'C)
cDef = cfg cSumTagNullTerm

-- | Special generic derivation configuration you may use for non-sum data
--   types.
--
-- When generically deriving binrep instances for a non-sum type, you may like
-- to ignore sum tag handling. You could use 'cDef', but this will silently
-- change behaviour if your type becomes a sum type. This configuration will
-- generate clear runtime errors when used with a sum type.
--
-- By selecting 'Void' for the sum tag type, consumption actions (serializing,
-- getting length in bytes) will runtime error, while generation actions
-- (parsing) will hit the 'Void' instance first and always safely error out.
cNoSum :: Cfg Void
cNoSum = cfg $ \_ -> throw EDerivedSumInstanceWithNonSumCfg

-- This indirection enables us to test for this precise exception being thrown
-- in an incorrect configuration! Awesome!
data EDerivedSumInstanceWithNonSumCfg = EDerivedSumInstanceWithNonSumCfg
instance Show EDerivedSumInstanceWithNonSumCfg where
    show EDerivedSumInstanceWithNonSumCfg =
        "Binrep.Generic.cNoSum: non-sum generic derivation configuration used with a sum type"
instance Exception EDerivedSumInstanceWithNonSumCfg
