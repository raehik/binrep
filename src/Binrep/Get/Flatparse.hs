{-# LANGUAGE UndecidableInstances #-} -- for 'TypeError'

module Binrep.Get.Flatparse
  ( Getter, Get(..), runGet, runGetter
  , E(..), EBase(..), EGeneric(..), EGenericSum(..)
  , eBase
  , getEWrap, getEBase
  , cutEBase
  -- , GetWith(..), runGetWith
  , getGenericNonSum, getGenericSum
  ) where

import FlatParse.Basic qualified as FP
import Data.ByteString qualified as B

import Binrep.Util.Class
import GHC.TypeError

import Data.Void
import Data.Word
import Data.Int
import Bytezap
import Bytezap.Bytes qualified as BZ

import Data.Text ( Text )

import Numeric.Natural

import GHC.Generics ( Generic, type Rep )
import Senserial.Sequential.Parse.NonSum qualified as Senserial
import Senserial.Sequential.Parse.Sum qualified as Senserial
import Senserial.Sequential.Parse.Internal.Parser qualified as Senserial

type Getter a = FP.Parser E a

data E
  = EBase EBase

  | EGeneric String {- ^ datatype name -} EGeneric

    deriving stock (Eq, Show, Generic)

eBase :: EBase -> Getter a
eBase = FP.err . EBase

-- | TODO confirm correct operation (error combination)
getEWrap :: Get a => (E -> E) -> Getter a
getEWrap f = FP.cutting get (f $ EBase EFail) (\e _ -> f e)

getEBase :: Get a => EBase -> Getter a
getEBase = FP.cut get . EBase

cutEBase :: Getter a -> EBase -> Getter a
cutEBase f e = FP.cut f $ EBase e

data EBase
  = EFail
  -- ^ TODO ? maybe unannotated error? idk.

  | EExpectedByte Word8 Word8
  -- ^ expected first, got second

  | EOverlong Int Int
  -- ^ expected first, got second

  | EExpected B.ByteString B.ByteString
  -- ^ expected first, got second

  | EFailNamed String
  -- ^ known fail

  | EFailParse String B.ByteString Word8
  -- ^ parse fail (where you parse a larger object, then a smaller one in it)

  | ERanOut Natural
  -- ^ ran out of input, needed precisely @n@ bytes for this part (n > 0)

    deriving stock (Eq, Show, Generic)

data EGeneric
  = EGenericSum EGenericSum
  | EGenericField String (Maybe String) Natural E
    deriving stock (Eq, Show, Generic)

data EGenericSum
  = EGenericSumTag E
  | EGenericSumTagNoMatch [String] Text
    deriving stock (Eq, Show, Generic)

class Get a where
    -- | Parse from binary.
    get :: Getter a

runGet :: Get a => B.ByteString -> Either E (a, B.ByteString)
runGet = runGetter get

runGetter :: Getter a -> B.ByteString -> Either E (a, B.ByteString)
runGetter g bs = case FP.runParser g bs of
                   FP.OK a bs' -> Right (a, bs')
                   FP.Fail     -> Left $ EBase EFail
                   FP.Err e    -> Left e

instance Senserial.SeqParser (FP.Parser E) where
    type SeqParserC (FP.Parser E) = Get
    seqParse cd cc mcs si = getEWrap $ EGeneric cd . EGenericField cc mcs si

instance Senserial.SeqParserSum (FP.Parser E) where
    seqParserSumParsePfxTag cd =
        getEWrap $ EGeneric cd . EGenericSum . EGenericSumTag
    seqParserSumErrNoMatchingCstr cd cstrs ptText =
        FP.err $ EGeneric cd $ EGenericSum $ EGenericSumTagNoMatch cstrs ptText

type GGetVia f a = f (FP.Parser E) (Rep a)

getGenericSum
    :: forall pt a
    .  (Generic a, GGetVia Senserial.SeqParseSum a, Get pt)
    => Senserial.PfxTagCfg pt -> Getter a
getGenericSum = Senserial.seqParseSum

getGenericNonSum
    :: (Generic a, GGetVia Senserial.SeqParseNonSum a)
    => Getter a
getGenericNonSum = Senserial.seqParseNonSum

instance TypeError ENoEmpty => Get Void where get = undefined
instance TypeError ENoSum => Get (Either a b) where get = undefined

-- | Parse a bytestring and... immediate reserialize it.
--
-- Note that this _does_ perform work: we make a new bytestring so we don't rely
-- on the input bytestring. To use the input bytestring directly, see
-- "Binrep.Type.Thin".
instance Get Write where
    {-# INLINE get #-}
    get = fmap BZ.byteString $ fmap B.copy $ FP.takeRest

-- | Unit type parses nothing.
instance Get () where
    {-# INLINE get #-}
    get = pure ()

-- | Parse tuples left-to-right.
instance (Get l, Get r) => Get (l, r) where
    {-# INLINE get #-}
    get = do
        l <- get
        r <- get
        pure (l, r)

-- | Parse elements until EOF. Sometimes used at the "top" of binary formats.
instance Get a => Get [a] where
    get = go
      where
        go = do
            FP.withOption FP.eof (\() -> pure []) $ do
                a <- get
                as <- go
                pure $ a : as

-- | Return the rest of the input.
--
-- A plain unannotated bytestring isn't very useful -- you'll usually want to
-- null-terminate or length-prefix it.
--
-- Note that this _does_ perform work: we make a new bytestring so we don't rely
-- on the input bytestring. To use the input bytestring directly, see
-- "Binrep.Type.Thin".
instance Get B.ByteString where
    {-# INLINE get #-}
    get = B.copy <$> FP.takeRest

-- | Unsigned byte.
instance Get Word8 where get = cutEBase FP.anyWord8 (ERanOut 1)

-- | Signed byte.
instance Get  Int8 where get = cutEBase FP.anyInt8  (ERanOut 1)

{-
Multi-byte machine integers require an endianness to use. A common wrapper is
defined in "Binrep.Type.Int".
-}

{-

-- | A type that can be parsed from binary given some environment.
--
-- Making this levity polymorphic makes things pretty strange, but is useful.
-- See @Binrep.Example.FileTable@.
class GetWith (r :: TYPE rep) a | a -> r where
    -- | Parse from binary with the given environment.
    getWith :: r -> Getter a
    -- can no longer provide default implementation due to levity polymorphism
    --default getWith :: Get a => r -> Getter a
    --getWith _ = get

--deriving anyclass instance Get a => GetWith r [a]

-- Note that @r@ is not levity polymorphic, GHC forces it to be lifted. You
-- can't bind (LHS) a levity polymorphic value.
runGetWith
    :: GetWith (r :: TYPE LiftedRep) a
    => r -> B.ByteString -> Either E (a, B.ByteString)
runGetWith r bs = runGetter (getWith r) bs

-}
