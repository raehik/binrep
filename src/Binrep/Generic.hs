module Binrep.Generic
  (
  -- * Shared configuration
    Cfg(..), cfg, cSumTagHex, cSumTagNullTerm, cDef
  -- * BLen
  , blenGenericSum, BLen.blenGenericNonSum
  -- * Put
  , putGenericSum', Put.putGenericNonSum
  -- * Get
  , Get.getGenericSum, Get.getGenericNonSum
  -- * CBLen
  , type CBLen.CBLenGeneric
  ) where

import Binrep
import Bytezap ( Poke )
import Binrep.BLen.Simple.Generic qualified as BLen
import Binrep.Put.Bytezap qualified as Put
import Binrep.Get.Flatparse qualified as Get
import Binrep.CBLen.Generic qualified as CBLen

import Binrep.Type.NullTerminated
import Data.ByteString qualified as B
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Refined.Unsafe
import Numeric ( readHex )
import Binrep.Util ( tshow )

import GHC.Generics ( Generic, type Rep )
import Senserial.Sequential.Serialize.Sum qualified as Senserial

import Data.Text ( Text )

putGenericSum'
    :: (Generic a, Put.GPutVia Senserial.GSeqSerDSum a, Put w) => Cfg w -> a -> Poke
putGenericSum' c = Put.putGenericSum (put . cSumTag c)

blenGenericSum
    :: (Generic a, BLen.GBLenDSum (Rep a), BLen w) => Cfg w -> a -> Int
blenGenericSum c = BLen.blenGenericSum (blen . cSumTag c)

data Cfg a = Cfg
  { cSumTag :: String -> a
  -- ^ How to turn a constructor name into a prefix tag.

  , cSumTagEq   :: a -> a -> Bool
  -- ^ How to compare prefix tags for equality.
  --
  -- By shoving this into our generic derivation config, we can avoid adding an
  -- insidious 'Eq' constraint. In general, you will want to set this to '(==)'.

  , cSumTagShow :: a -> Text
  }

-- | Construct a binrep generic deriving config, filling out the relevant
--   records using 'Eq' and 'Show'.
cfg :: (Eq a, Show a) => (String -> a) -> Cfg a
cfg f = Cfg { cSumTag = f, cSumTagEq = (==), cSumTagShow = tshow }

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
cSumTagNullTerm :: String -> NullTerminated B.ByteString
cSumTagNullTerm = reallyUnsafeRefine . Text.encodeUtf8 . Text.pack

-- | Default generic derivation configuration, using 'cSumTagNullTerm'.
cDef :: Cfg (NullTerminated B.ByteString)
cDef = cfg cSumTagNullTerm
