{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE BlockArguments #-}

module Binrep.Get
  ( Getter, Get(..), runGet, runGetter
  , E(..), EBase(..), EGeneric(..), EGenericSum(..)
  , eBase
  , getEBase
  -- , GetWith(..), runGetWith
  , getPrim
  , getGenericNonSum, getGenericSum
  ) where

import Data.Functor.Identity
import Binrep.Util.ByteOrder
import Binrep.Via.Prim ( ViaPrim(..) )
import Raehik.Compat.Data.Primitive.Types ( Prim', sizeOf )
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )

import FlatParse.Basic qualified as FP
import Raehik.Compat.FlatParse.Basic.Prim qualified as FP

import Data.ByteString qualified as B

import Binrep.Util.Class
import GHC.TypeLits ( TypeError )

import Data.Void
import Data.Word
import Data.Int

import Data.Text ( Text )

import Numeric.Natural

import GHC.Generics
import Generic.Data.Function.Traverse
import Generic.Data.Function.Common
import Generic.Data.Rep.Assert

import GHC.Exts ( minusAddr#, Int(I#) )

type Getter a = FP.Parser E a

-- | Structured parse error.
data E
  = E Int EMiddle

  -- | Unhandled parse error.
  --
  -- You get this if you don't change a flatparse fail to an error.
  --
  -- Should not be set except by library code.
  | EFail

    deriving stock (Eq, Show, Generic)

data EMiddle

  -- | Parse error with no further context.
  = EBase EBase

  -- | Somehow, we got two parse errors.
  --
  -- I have a feeling that seeing this indicates a problem in your code.
  | EAnd E EBase

  -- | Parse error decorated with generic info.
  --
  -- Should not be set except by library code.
  | EGeneric String {- ^ data type name -} (EGeneric E)

    deriving stock (Eq, Show, Generic)

data EBase
  = EExpectedByte Word8 Word8
  -- ^ expected first, got second

  | EOverlong Int Int
  -- ^ expected first, got second

  | EExpected B.ByteString B.ByteString
  -- ^ expected first, got second

  | EFailNamed String
  -- ^ known fail

  | EFailParse String B.ByteString Word8
  -- ^ parse fail (where you parse a larger object, then a smaller one in it)

  | ERanOut Int
  -- ^ ran out of input, needed precisely @n@ bytes for this part (n > 0)
  --
  -- Actually a 'Natural', but we use 'Int' because that's what flatparse uses
  -- internally.

    deriving stock (Eq, Show, Generic)

-- | A generic context layer for a parse error of type @e@.
--
-- Recursive: parse errors occurring in fields are wrapped up here. (Those
-- errors may also have a generic context layer.)
--
-- Making this explicitly recursive may seem strange, but it clarifies that this
-- data type is to be seen as a layer over a top-level type.
data EGeneric e
  -- | Parse error relating to sum types (constructors).
  = EGenericSum (EGenericSum e)

  -- | Parse error in a constructor field.
  | EGenericField
        String          -- ^ constructor name
        (Maybe String)  -- ^ field record name (if present)
        Natural         -- ^ field index in constructor
        e               -- ^ field parse error
    deriving stock (Eq, Show, Generic)

data EGenericSum e
  -- | Parse error parsing prefix tag.
  = EGenericSumTag e

  -- | Unable to match a constructor to the parsed prefix tag.
  | EGenericSumTagNoMatch
        [String] -- ^ constructors tested
        Text     -- ^ prettified prefix tag
    deriving stock (Eq, Show, Generic)

eBase :: EBase -> Getter a
eBase eb = FP.ParserT \_fp eob s st ->
    let os = I# (minusAddr# eob s)
     in FP.Err# st (E os $ EBase eb)

getEBase :: Getter a -> EBase -> Getter a
getEBase (FP.ParserT f) eb =
    FP.ParserT \fp eob s st ->
        let os = I# (minusAddr# eob s)
         in case f fp eob s st of
              FP.Fail# st'   -> FP.Err# st' (E os $ EBase eb)
              FP.Err#  st' e -> FP.Err# st' (E os $ EAnd e eb)
              x -> x

-- | Parse. On parse error, coat it in a generic context layer.
getWrapGeneric :: Get a => String -> (E -> EGeneric E) -> Getter a
getWrapGeneric = getWrapGeneric' get

getWrapGeneric' :: Getter a -> String -> (E -> EGeneric E) -> Getter a
getWrapGeneric' (FP.ParserT f) cd fe =
    FP.ParserT \fp eob s st ->
        let os = I# (minusAddr# eob s)
         in case f fp eob s st of
              FP.Fail# st'   -> FP.Err# st' (E os $ EGeneric cd $ fe EFail)
              FP.Err#  st' e -> FP.Err# st' (E os $ EGeneric cd $ fe e)
              x -> x

class Get a where
    -- | Parse from binary.
    get :: Getter a

runGet :: Get a => B.ByteString -> Either E (a, B.ByteString)
runGet = runGetter get

runGetter :: Getter a -> B.ByteString -> Either E (a, B.ByteString)
runGetter g bs = case FP.runParser g bs of
                   FP.OK a bs' -> Right (a, bs')
                   FP.Fail     -> Left EFail
                   FP.Err e    -> Left e

instance GenericTraverse (FP.Parser E) where
    type GenericTraverseC (FP.Parser E) a = Get a
    genericTraverseAction cd cc mcs si =
        getWrapGeneric cd $ EGenericField cc mcs si

instance GenericTraverseSum (FP.Parser E) where
    genericTraverseSumPfxTagAction cd =
        getWrapGeneric cd $ EGenericSum . EGenericSumTag
    -- TODO proper offset info
    genericTraverseSumNoMatchingCstrAction cd cstrs ptText =
        FP.err $ E 0 $ EGeneric cd $ EGenericSum $ EGenericSumTagNoMatch cstrs ptText

getGenericNonSum
    :: forall {cd} {f} {asserts} a
    .  (Generic a, Rep a ~ D1 cd f, GTraverseNonSum cd (FP.Parser E) f
       , asserts ~ '[ 'NoEmpty, 'NoSum], ApplyGCAsserts asserts f)
    => Getter a
getGenericNonSum = genericTraverseNonSum @asserts

getGenericSum
    :: forall {cd} {f} {asserts} pt a
    .  ( Generic a, Rep a ~ D1 cd f, GTraverseSum 'SumOnly cd (FP.Parser E) f
       , Get pt
       , asserts ~ '[ 'NoEmpty, 'NeedSum], ApplyGCAsserts asserts f)
    => PfxTagCfg pt -> Getter a
getGenericSum = genericTraverseSum @'SumOnly @asserts

instance TypeError ENoEmpty => Get Void where get = undefined
instance TypeError ENoSum => Get (Either a b) where get = undefined

{-

-- | Parse a bytestring and... immediate reserialize it.
--
-- Note that this _does_ perform work: we make a new bytestring so we don't rely
-- on the input bytestring. To use the input bytestring directly, see
-- "Binrep.Type.Thin".
instance Get Write where
    {-# INLINE get #-}
    get = fmap BZ.byteString $ fmap B.copy $ FP.takeRest

-}

instance Get a => Get (Identity a) where get = Identity <$> get

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

-- | 8-bit (1-byte) words do not require byte order in order to precisely
--   define their representation.
deriving via ViaPrim Word8 instance Get Word8

-- | 8-bit (1-byte) words do not require byte order in order to precisely
--   define their representation.
deriving via ViaPrim  Int8 instance Get  Int8

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via Identity Word8 instance Get (ByteOrdered end Word8)

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via Identity  Int8 instance Get (ByteOrdered end  Int8)

-- | Parse any 'Prim''.
getPrim :: forall a. Prim' a => Getter a
getPrim = getEBase FP.anyPrim (ERanOut (sizeOf (undefined :: a)))

instance Prim' a => Get (ViaPrim a) where get = ViaPrim <$> getPrim

-- ByteSwap is required on opposite endian platforms, but we're not checking
-- here, so make sure to keep it on both.
deriving via ViaPrim (ByteOrdered 'LittleEndian a)
    instance (Prim' a, ByteSwap a) => Get (ByteOrdered 'LittleEndian a)
deriving via ViaPrim (ByteOrdered    'BigEndian a)
    instance (Prim' a, ByteSwap a) => Get (ByteOrdered    'BigEndian a)

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
