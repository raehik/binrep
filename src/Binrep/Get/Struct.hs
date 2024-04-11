{-# LANGUAGE UndecidableInstances #-} -- for Generically instance

module Binrep.Get.Struct where

import Binrep.Get.Error
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
import Data.Functor.Identity
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )

import Data.ByteString qualified as B

import Generic.Type.Assert

import Binrep.Common.Via.Generically.NonSum

type GetterC = Parser E

-- | constant size parser
class GetC a where getC :: GetterC a

runGetCBs
    :: forall a. (GetC a, KnownNat (CBLen a))
    => B.ByteString -> Either E a
runGetCBs bs =
    if   cblen @a <= B.length bs
    then unsafeRunGetC' unsafeRunParserBs bs
    else Left $ E 0 $ EBase $ ERanOut 0 -- TODO made up numbers

-- | doesn't check len
unsafeRunGetC'
    :: forall a buf. GetC a
    => (forall e. buf -> Parser e a -> Result e a)
    -> buf -> Either E a
unsafeRunGetC' p buf =
    case p buf getC of
      OK   a -> Right a
      Fail   -> Left EFail
      Err  e -> Left e

-- | doesn't check len
unsafeRunGetCPtr
    :: forall a. GetC a
    => Ptr Word8 -> Either E a
unsafeRunGetCPtr = unsafeRunGetC' unsafeRunParserPtr

instance GParseBase GetC where
    type GParseBaseSt GetC = Proxy# Void
    type GParseBaseC  GetC a = GetC a
    type GParseBaseE  GetC = E
    gParseBase = getC
    type GParseBaseLenTF GetC = CBLenSym

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

instance GetC () where
    {-# INLINE getC #-}
    getC = constParse ()

instance Prim' a => GetC (ViaPrim a) where
    getC = ViaPrim <$> prim
    {-# INLINE getC #-}

instance GetC a => GetC (Identity a) where getC = Identity <$> getC

deriving via ViaPrim Word8 instance GetC Word8
deriving via ViaPrim  Int8 instance GetC  Int8
deriving via Word8 instance GetC (ByteOrdered end Word8)
deriving via  Int8 instance GetC (ByteOrdered end  Int8)

-- ByteSwap is required on opposite endian platforms, but we're not checking
-- here, so make sure to keep it on both.
deriving via ViaPrim (ByteOrdered 'LittleEndian a)
    instance (Prim' a, ByteSwap a) => GetC (ByteOrdered 'LittleEndian a)
deriving via ViaPrim (ByteOrdered    'BigEndian a)
    instance (Prim' a, ByteSwap a) => GetC (ByteOrdered    'BigEndian a)

{-

instance TypeError ENoEmpty => PutC Void where putC = undefined
instance TypeError ENoSum => PutC (Either a b) where putC = undefined

instance PutC a => PutC (Identity a) where putC = putC . runIdentity

instance PutC PutterC where putC = id

-- | Look weird? Yeah. But it's correct :)
instance (PutC l, KnownNat (CBLen l), PutC r) => PutC (l, r) where
    {-# INLINE putC #-}
    putC (l, r) = sequencePokes (putC l) (cblen @l) (putC r)

-}

eCBase :: EBase -> GetterC a
eCBase eb = ParserT $ \_fpc _base os# st ->
    Err# st (E (I# os#) $ EBase eb)

getECBase :: GetterC a -> EBase -> GetterC a
getECBase (ParserT p) eb = ParserT $ \fpc base os# st0 ->
    case p fpc base os# st0 of
      Fail# st1   -> Err# st1 (E (I# os#) $ EBase eb)
      Err#  st1 e -> Err# st1 (E (I# os#) $ EAnd e eb)
      x -> x
