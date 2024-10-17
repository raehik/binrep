{-# LANGUAGE UndecidableInstances #-} -- for various stuff
{-# LANGUAGE AllowAmbiguousTypes #-} -- for type-level sum type handling
{-# LANGUAGE OverloadedStrings #-} -- for easy error building

module Binrep.Get
  ( module Binrep.Get
  , module Binrep.Get.Error
  ) where

import Binrep.Get.Error
import Data.Text.Builder.Linear qualified as TBL
import GHC.Exts ( fromString )
import Binrep.Util.ByteOrder
import Binrep.Common.Via.Prim ( ViaPrim(..) )
import Raehik.Compat.Data.Primitive.Types ( Prim', sizeOf )
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )

import Binrep.Get.Struct ( GetC(getC), GetterC )
import Bytezap.Parser.Struct qualified as BZ
import Binrep.CBLen ( IsCBLen(CBLen), cblen )
import GHC.TypeLits ( KnownNat )

import FlatParse.Basic qualified as FP
import Raehik.Compat.FlatParse.Basic.Prim qualified as FP
import Raehik.Compat.FlatParse.Basic.CutWithPos qualified as FP
import Raehik.Compat.FlatParse.Basic.Remaining qualified as FP

import Data.ByteString qualified as B

import Binrep.Common.Class.TypeErrors ( ENoSum, ENoEmpty )
import GHC.TypeLits ( TypeError )

import GHC.Generics
import Generic.Data.Function.Traverse
import Generic.Data.MetaParse.Cstr ( Raw, ParseCstrTo )
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

import Data.Typeable ( Typeable, TypeRep, typeRep, Proxy(Proxy) )

type Getter = FP.Parser (ParseError FP.Pos TBL.Builder)

class Get a where
    -- | Parse from binary.
    get :: Getter a

runGet
    :: Get a
    => B.ByteString
    -> Either (ParseError Int TBL.Builder) (a, B.ByteString)
runGet = runGetter get

runGetter
    :: Getter a
    -> B.ByteString
    -> Either (ParseError Int TBL.Builder) (a, B.ByteString)
runGetter p bs =
    case FP.runParser p bs of
      FP.OK   a bs' -> Right (a, bs')
      FP.Err  e     ->
        -- TODO check this is right. might need length of bs' ... ?
        Left $ fmap (mapParseErrorSinglePos (\(FP.Pos pos) -> len - pos)) e
      FP.Fail       -> Left []
  where len = B.length bs

instance GenericTraverse Get where
    type GenericTraverseF Get = Getter
    type GenericTraverseC Get a = Get a
    genericTraverseAction dtName cstrName mFieldName fieldIdx =
        get `cutting1` e
      where
        e = parseErrorTextGenericFieldBld dtName cstrName mFieldName fieldIdx

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
    :: forall sumtag pt a
    .  ( Generic a, GTraverseSum Get sumtag (Rep a)
       , Get pt
       , GAssertNotVoid a, GAssertSum a
    ) => ParseCstrTo sumtag pt
      -> (pt -> pt -> Bool)
      -> Getter a
getGenericSum parseCstr ptEq =
    genericTraverseSum @Get @sumtag parseCstr ptGet fNoMatch ptEq
  where
      fNoMatch dtName = err1 (parseErrorTextGenericNoCstrMatchBld dtName)
      ptGet dtName = get `cutting1` parseErrorTextGenericSumTagBld dtName

getGenericSumRaw
    :: forall pt a
    .  ( Generic a, GTraverseSum Get Raw (Rep a)
       , Get pt
       , GAssertNotVoid a, GAssertSum a
    ) => (String -> pt)
      -> (pt -> pt -> Bool)
      -> Getter a
getGenericSumRaw parseCstr ptEq =
    genericTraverseSumRaw @Get parseCstr ptGet fNoMatch ptEq
  where
      fNoMatch dtName = err1 (parseErrorTextGenericNoCstrMatchBld dtName)
      ptGet dtName = get `cutting1` parseErrorTextGenericSumTagBld dtName

-- | Emit a single error. Use with flatparse primitives that only 'FP.Fail'.
err1 :: [text] -> FP.ParserT st (ParseError FP.Pos text) a
err1 = FP.err' . parseError1

-- | Turn a 'FP.Fail' into a single error. (Re-emits existing 'FP.Error's.)
--
-- Use when wrapping flatparse primitives that directly only 'FP.Fail'. (It's
-- fine to use with combinators if the combinator itself doesn't 'FP.Error'.)
cut1
    :: FP.ParserT st (ParseError FP.Pos text) a -> [text]
    -> FP.ParserT st (ParseError FP.Pos text) a
cut1 p texts = p `FP.cut'` parseError1 texts

-- | Turn a 'FP.Fail' into a single error, or prepend it to any existing ones.
--
-- Use when wrapping other 'get'ters.
--
-- We reimplement 'FP.cutting' with a tweak. Otherwise, we'd have to join lists
-- in the error case (instead of simply prepending).
cutting1
    :: FP.ParserT st (ParseError FP.Pos text) a -> [text]
    -> FP.ParserT st (ParseError FP.Pos text) a
cutting1 (FP.ParserT p) texts =
    FP.getPos >>= \pos -> FP.ParserT $ \fp eob s st ->
        case p fp eob s st of
          FP.Fail# st'    -> FP.Err# st' [ParseErrorSingle pos texts]
          FP.Err#  st' e' -> FP.Err# st' (ParseErrorSingle pos texts : e')
          x            -> x

-- We can't provide a Generically instance because the user must choose between
-- sum and non-sum handlers.

instance GenericFOnCstr Get where
    type GenericFOnCstrF Get = Getter
    type GenericFOnCstrC Get dtName cstrName gf =
        GTraverseC Get dtName cstrName 0 gf
    genericFOnCstrF (_ :: Proxy# '(dtName, cstrName)) =
        gTraverseC @Get @dtName @cstrName @0

-- TODO this is hard to parse visually. document...?
fpToBz
    :: FP.ParserT st (ParseError FP.Pos text) a -> Int#
    -> (a -> Int# -> BZ.ParserT st (ParseError Int text) r)
    -> BZ.ParserT st (ParseError Int text) r
fpToBz (FP.ParserT p) len# fp = BZ.ParserT $ \fpc base# os# st0 ->
    case p fpc (base# `plusAddr#` (os# +# len#)) (base# `plusAddr#` os#) st0 of
      FP.OK#   st1 a s ->
        let unconsumed# = s `minusAddr#` (base# `plusAddr#` os#)
        in  BZ.runParserT# (fp a unconsumed#) fpc base# (os# +# unconsumed#) st1
      FP.Err#  st1 e   ->
        -- on error, we turn the flatparse 'FP.Pos' indices into actual byte
        -- offsets (which bytezap deals in), then emit
        let e' = fmap (mapParseErrorSinglePos (\(FP.Pos pos) -> I# len# - pos)) e
        in  BZ.Err# st1 e'
      FP.Fail# st1     -> BZ.Fail# st1

newtype ViaGetC a = ViaGetC { unViaGetC :: a }
instance (GetC a, KnownNat (CBLen a)) => Get (ViaGetC a) where
    {-# INLINE get #-}
    get = ViaGetC <$> bzToFp getC

-- TODO messy ran out of input handling. should be a util for it
-- TODO pos handling seems correct on quick test. need stronger assertion plz
bzToFp :: forall a. KnownNat (CBLen a) => GetterC a -> Getter a
bzToFp (BZ.ParserT p) =
    (FP.ensure (I# len#) `cut1` eRanOut) >> FP.getPos >>= \(FP.Pos pos) ->
        FP.ParserT $ \fpc _eob s st0 ->
            case p fpc s 0# st0 of
              BZ.OK#   st1 a -> FP.OK#   st1 a (s `plusAddr#` len#)
              BZ.Err#  st1 e ->
                let e' = fmap (mapParseErrorSinglePos (\idx -> FP.Pos (pos - idx))) e
                in  FP.Err# st1 e'
              BZ.Fail# st1   -> FP.Fail# st1
  where
    !(I# len#) = cblen @a
    eRanOut = [ "ran out of input while running inner parser"
              , "bytes needed: "<>TBL.fromDec (I# len#) ]

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
    -- TODO slow, uses reverse. build a DList instead
    get = go []
      where
        go as = FP.branch FP.eof (pure (reverse as)) (get >>= \a -> go (a : as))

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
getPrim :: forall a. (Prim' a, Typeable a) => Getter a
getPrim = do
    lenAvail <- FP.remaining
    FP.anyPrim `cut1`
        [  "ran out of bytes while parsing " <> strTR
        <> ", needed "    <> strLenNeed
        <> ", remaining " <> TBL.fromDec lenAvail
        ]
  where
    strTR       = fromString (show (typeRep' @a))
    strLenNeed  = TBL.fromDec (sizeOf (undefined :: a))

typeRep' :: forall a. Typeable a => TypeRep
typeRep' = typeRep (Proxy @a)

instance (Prim' a, Typeable a) => Get (ViaPrim a) where
    get = ViaPrim <$> getPrim

-- ByteSwap is required on opposite endian platforms, but we're not checking
-- here, so make sure to keep it on both.
deriving via ViaPrim (ByteOrdered LittleEndian a)
    instance (Prim' a, ByteSwap a, Typeable a)
      => Get (ByteOrdered LittleEndian a)
deriving via ViaPrim (ByteOrdered    BigEndian a)
    instance (Prim' a, ByteSwap a, Typeable a)
      => Get (ByteOrdered    BigEndian a)

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
