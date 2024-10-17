{-# LANGUAGE UndecidableInstances #-} -- for Generically instance
{-# LANGUAGE OverloadedStrings #-} -- for easy error building

module Binrep.Get.Struct
  ( GetterC, GetC(getC)
  , getGenericStruct
  , runGetCBs
  , unsafeRunGetCPtr
  ) where

import Binrep.Get.Error
import Data.Text.Builder.Linear qualified as TBL
import Bytezap.Parser.Struct
import Bytezap.Parser.Struct.Generic
import Binrep.CBLen
import Foreign.Ptr ( Ptr )
import Data.Void ( Void )
import GHC.Exts ( Proxy#, Int(I#) )
import GHC.TypeNats ( KnownNat )
import GHC.Generics

import Binrep.Common.Via.Prim ( ViaPrim(..) )
import Raehik.Compat.Data.Primitive.Types ( Prim' )

import Data.Word ( Word8 )
import Data.Int ( Int8 )
import Binrep.Util.ByteOrder
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )

import Data.ByteString qualified as B

import Generic.Type.Assert

import Binrep.Common.Via.Generically.NonSum

import Rerefined.Refine
import Rerefined.Predicate.Logical.And

type GetterC = Parser (ParseError Int TBL.Builder)

-- | constant size parser
class GetC a where getC :: GetterC a

-- | Consume 'Result'.
finishGetterC
    :: Result (ParseError Int TBL.Builder) a
    -> Either (ParseError Int TBL.Builder) a
finishGetterC = \case
  OK  a -> Right a
  Err e -> Left  e
  Fail  -> Left  []

runGetCBs
    :: forall a. (GetC a, KnownNat (CBLen a))
    => B.ByteString -> Either (ParseError Int TBL.Builder) a
runGetCBs bs =
    if   lenReq <= lenAvail
    then finishGetterC $ unsafeRunParserBs bs getC
    else Left [ParseErrorSingle 0 [errMsg]]
  where
    lenReq   = cblen @a
    lenAvail = B.length bs
    errMsg   =
        "input too short (need "<>TBL.fromDec lenReq
                      <>", got "<>TBL.fromDec lenAvail<>")"

-- | doesn't check len
unsafeRunGetCPtr
    :: forall a. GetC a
    => Ptr Word8 -> Either (ParseError Int TBL.Builder) a
unsafeRunGetCPtr ptr = finishGetterC $ unsafeRunParserPtr ptr getC

instance GParseBase GetC where
    type GParseBaseSt GetC = Proxy# Void
    type GParseBaseC  GetC a = GetC a
    type GParseBaseE  GetC = ParseError Int TBL.Builder
    gParseBase dtName cstrName mFieldName fieldIdx = getC `cutting1` e
      where
        e = parseErrorTextGenericFieldBld dtName cstrName mFieldName fieldIdx
    type GParseBaseLenTF GetC = CBLenSym

-- | Turn a 'Fail' into a single error, or prepend it to any existing ones.
--
-- Use when wrapping other 'get'ters.
--
-- We reimplement @cutting@ with a tweak. Otherwise, we'd have to join lists in
-- the error case (instead of simply prepending).
cutting1
    :: ParserT st (ParseError Int text) a -> [text]
    -> ParserT st (ParseError Int text) a
cutting1 (ParserT p) texts = ParserT $ \fpc base# os# st ->
    case p fpc base# os# st of
      Fail# st'    -> Err# st' [ParseErrorSingle (I# os#) texts]
      Err#  st' e' -> Err# st' (ParseErrorSingle (I# os#) texts : e')
      x               -> x

-- | Serialize a term of the struct-like type @a@ via its 'Generic' instance.
getGenericStruct
    :: forall a
    .  ( Generic a, GParse GetC (Rep a)
       , GAssertNotVoid a, GAssertNotSum a
    ) => GetterC a
getGenericStruct = to <$> gParse @GetC

instance
  ( Generic a, GParse GetC (Rep a)
  , GAssertNotVoid a, GAssertNotSum a
  ) => GetC (Generically a) where
    getC = Generically <$> getGenericStruct

instance
  ( Generic a, GParse GetC (Rep a)
  , GAssertNotVoid a, GAssertNotSum a
  ) => GetC (GenericallyNonSum a) where
    getC = GenericallyNonSum <$> getGenericStruct

instance GetC (Refined pr (Refined pl a))
  => GetC (Refined (pl `And` pr) a) where
    getC = (unsafeRefine . unrefine @pl . unrefine @pr) <$> getC

instance GetC () where
    {-# INLINE getC #-}
    getC = constParse ()

instance Prim' a => GetC (ViaPrim a) where
    getC = ViaPrim <$> prim
    {-# INLINE getC #-}

deriving via ViaPrim Word8 instance GetC Word8
deriving via ViaPrim  Int8 instance GetC  Int8
deriving via Word8 instance GetC (ByteOrdered end Word8)
deriving via  Int8 instance GetC (ByteOrdered end  Int8)

-- ByteSwap is required on opposite endian platforms, but we're not checking
-- here, so make sure to keep it on both.
deriving via ViaPrim (ByteOrdered LittleEndian a)
    instance (Prim' a, ByteSwap a) => GetC (ByteOrdered LittleEndian a)
deriving via ViaPrim (ByteOrdered    BigEndian a)
    instance (Prim' a, ByteSwap a) => GetC (ByteOrdered    BigEndian a)
