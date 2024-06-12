{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE BlockArguments #-}

module Binrep.Get
  ( module Binrep.Get
  , module Binrep.Get.Error
  ) where

import Binrep.Get.Error
import Binrep.Util.ByteOrder
import Binrep.Common.Via.Prim ( ViaPrim(..) )
import Raehik.Compat.Data.Primitive.Types ( Prim', sizeOf )
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )

import Binrep.Get.Struct ( GetC(getC) )
import Bytezap.Parser.Struct qualified as BZ
import Binrep.CBLen ( IsCBLen(CBLen), cblen )
import GHC.TypeLits ( KnownNat )

import FlatParse.Basic qualified as FP
import Raehik.Compat.FlatParse.Basic.Prim qualified as FP

import Data.ByteString qualified as B

import Binrep.Common.Class.TypeErrors ( ENoSum, ENoEmpty )
import GHC.TypeLits ( TypeError )

import GHC.Generics
import Generic.Data.Function.Traverse
import Generic.Data.MetaParse.Cstr ( Raw )
import Generic.Type.Assert

import GHC.Exts ( minusAddr#, Int(I#), Int#, plusAddr#, (+#) )

import Rerefined.Refine
import Rerefined.Predicate.Logical.And

import Data.Word
import Data.Int
import Data.Void
import Data.Functor.Identity
import Binrep.Common.Via.Generically.NonSum

import Generic.Data.FOnCstr
import Generic.Data.Function.Traverse.Constructor hiding ( ENoEmpty )
import GHC.Exts ( Proxy# )

type Getter a = FP.Parser E a

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

instance GenericTraverse Get where
    type GenericTraverseF Get = FP.Parser E
    type GenericTraverseC Get a = Get a
    genericTraverseAction cd cc mcs si =
        getWrapGeneric cd $ EGenericField cc mcs si

{-
instance GenericTraverseSum Get where
    genericTraverseSumPfxTagAction cd =
        getWrapGeneric cd $ EGenericSum . EGenericSumTag
    -- TODO proper offset info
    genericTraverseSumNoMatchingCstrAction cd cstrs ptText =
        FP.err $ E 0 $ EGeneric cd $ EGenericSum $ EGenericSumTagNoMatch cstrs ptText
-}

getGenericNonSum
    :: forall a
    .  ( Generic a, GTraverseNonSum Get (Rep a)
       , GAssertNotVoid a, GAssertNotSum a
    ) => Getter a
getGenericNonSum = genericTraverseNonSum @Get

instance
  ( Generic a, GTraverseNonSum Get (Rep a)
  , GAssertNotVoid a, GAssertNotSum a
  ) => Get (GenericallyNonSum a) where
    get = GenericallyNonSum <$> getGenericNonSum

getGenericSum
    :: forall pt a
    .  ( Generic a, GTraverseSum Get Raw (Rep a)
       , Get pt
       , GAssertNotVoid a, GAssertSum a
    ) => (String -> pt)
      -> (String -> FP.Parser E pt)
      -> (pt -> pt -> Bool)
      -> Getter a
getGenericSum parseCstr ptGet ptEq =
    genericTraverseSumRaw @Get parseCstr ptGet fNoMatch ptEq
  where
      fNoMatch _cd = FP.err EFail -- TODO

-- We can't provide a Generically instance because the user must choose between
-- sum and non-sum handlers.

instance GenericFOnCstr Get where
    type GenericFOnCstrF Get = FP.Parser E
    type GenericFOnCstrC Get dtName cstrName gf =
        GTraverseC Get dtName cstrName 0 gf
    genericFOnCstrF (_ :: Proxy# '(dtName, cstrName)) =
        gTraverseC @Get @dtName @cstrName @0

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

-- | Convert a bytezap struct parser to a flatparse parser.
bzToFp
    :: forall a e st. KnownNat (CBLen a)
    => BZ.ParserT st e a -> FP.ParserT st e a
bzToFp (BZ.ParserT p) = FP.ensure (I# len#) >> (FP.ParserT $ \fpc _eob s st0 ->
    case p fpc s 0# st0 of
      BZ.OK#   st1 a -> FP.OK#   st1 a (s `plusAddr#` len#)
      BZ.Fail# st1   -> FP.Fail# st1
      BZ.Err#  st1 e -> FP.Err#  st1 e
    )
  where
    !(I# len#) = cblen @a

fpToBz
    :: FP.ParserT st e a -> Int#
    -> (a -> Int# -> BZ.ParserT st e r) -> BZ.ParserT st e r
fpToBz (FP.ParserT p) len# fp = BZ.ParserT $ \fpc base# os# st0 ->
    case p fpc (base# `plusAddr#` (os# +# len#)) (base# `plusAddr#` os#) st0 of
      FP.OK#   st1 a s ->
        let unconsumed# = s `minusAddr#` (base# `plusAddr#` os#)
        in  BZ.runParserT# (fp a unconsumed#) fpc base# (os# +# unconsumed#) st1
      FP.Fail# st1     -> BZ.Fail# st1
      FP.Err#  st1 e   -> BZ.Err#  st1 e

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

newtype ViaGetC a = ViaGetC { unViaGetC :: a }
instance (GetC a, KnownNat (CBLen a)) => Get (ViaGetC a) where
    {-# INLINE get #-}
    get = ViaGetC <$> bzToFp getC

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
deriving via Word8 instance Get (ByteOrdered end Word8)

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via  Int8 instance Get (ByteOrdered end  Int8)

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

instance Get (Refined pr (Refined pl a)) => Get (Refined (pl `And` pr) a) where
    get = (unsafeRefine . unrefine @pl . unrefine @pr) <$> get

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
