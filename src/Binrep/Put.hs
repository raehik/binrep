{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for generic data op instance
{-# LANGUAGE AllowAmbiguousTypes #-} -- TODO tmp

module Binrep.Put where

import Binrep.BLen ( BLen(blen) )
import Data.Functor.Identity
import Bytezap.Poke
import Bytezap.Struct qualified as Struct
import Bytezap.Struct.Generic qualified as Struct
import Raehik.Compat.Data.Primitive.Types ( Prim', sizeOf )
import Binrep.Util.ByteOrder
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )
import Binrep.Common.Via.Prim ( ViaPrim(..) )

import Data.ByteString qualified as B

import Binrep.Common.Class.TypeErrors ( ENoSum, ENoEmpty )
import GHC.TypeLits ( TypeError, KnownNat )

import Data.Void
import Data.Word
import Data.Int

import GHC.Generics
import Generic.Data.Function.FoldMap
import Generic.Data.Function.Common
import Generic.Data.Rep.Assert

import Control.Monad.ST ( RealWorld )

import Binrep.Common.Class.Generic ( BinrepG )
import Binrep.CBLen

type Putter  = Poke RealWorld
type PutterC = Struct.Poke RealWorld

class Put a where put :: a -> Putter

-- | constant size putter
class PutC a where putC :: a -> PutterC

runPut :: (BLen a, Put a) => a -> B.ByteString
runPut a = unsafeRunPokeBS (blen a) (put a)

-- TODO UGH I need to make my own internal idx here. Can't use 'Putter' since
-- that's just bytezap, which others may want to use differently with g-f-d.
-- Ah, but that means I need to redesign g-f-d!
instance GenericFoldMap Putter where
    type GenericFoldMapC Putter a = Put a
    genericFoldMapF = put

-- | Serialize a term of the non-sum type @a@ via its 'Generic' instance.
putGenericNonSum
    :: forall {cd} {f} {asserts} a
    .  ( Generic a, Rep a ~ D1 cd f, GFoldMapNonSum Putter f
       , asserts ~ '[ 'NoEmpty, 'NoSum], ApplyGCAsserts asserts f)
    => a -> Putter
putGenericNonSum = genericFoldMapNonSum @asserts

-- | Serialize a term of the sum type @a@ via its 'Generic' instance.
--
-- You must provide a serializer for @a@'s constructors. This is regrettably
-- inefficient due to having to use 'String's. Alas. Do write your own instance
-- if you want better performance!
putGenericSum
    :: forall {cd} {f} {asserts} a
    .  ( Generic a, Rep a ~ D1 cd f, GFoldMapSum 'SumOnly Putter f
       , asserts ~ '[ 'NoEmpty, 'NeedSum], ApplyGCAsserts asserts f)
    => (String -> Putter) -> a -> Putter
putGenericSum = genericFoldMapSum @'SumOnly @asserts

instance Struct.GPokeBase BinrepG where
    type GPokeBaseSt BinrepG = RealWorld
    type GPokeBaseC BinrepG a = PutC a
    gPokeBase = Struct.unPoke . putC
    type KnownSizeOf' BinrepG a = KnownNat (CBLen a)
    sizeOf' = reifyCBLenProxy#

putGenericStruct
    :: forall a
    .  ( Generic a, Struct.GPoke BinrepG (Rep a) )
    => a -> PutterC
putGenericStruct = Struct.Poke . Struct.gPoke @BinrepG . from

instance Prim' a => PutC (ViaPrim a) where
    putC = Struct.prim . unViaPrim
    {-# INLINE putC #-}

instance Prim' a => Put (ViaPrim a) where
    put = fromStructPoke (sizeOf (undefined :: a)) . putC
    {-# INLINE put #-}

instance TypeError ENoEmpty => Put Void where put = undefined
instance TypeError ENoSum => Put (Either a b) where put = undefined

instance Put a => Put (Identity a) where put = put . runIdentity

instance Put Putter where put = id

-- | Unit type serializes to nothing. How zen.
instance Put () where
    {-# INLINE put #-}
    put = mempty

instance (Put l, Put r) => Put (l, r) where
    {-# INLINE put #-}
    put (l, r) = put l <> put r

instance Put a => Put [a] where
    {-# INLINE put #-}
    put = mconcat . map put

instance Put B.ByteString where
    {-# INLINE put #-}
    put = byteString

-- | 8-bit (1-byte) words do not require byte order in order to precisely
--   define their representation.
deriving via ViaPrim Word8 instance Put Word8

-- | 8-bit (1-byte) words do not require byte order in order to precisely
--   define their representation.
deriving via ViaPrim  Int8 instance Put  Int8

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via Identity Word8 instance Put (ByteOrdered end Word8)

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via Identity  Int8 instance Put (ByteOrdered end  Int8)

-- ByteSwap is required on opposite endian platforms, but we're not checking
-- here, so make sure to keep it on both.
deriving via ViaPrim (ByteOrdered 'LittleEndian a)
    instance (Prim' a, ByteSwap a) => Put (ByteOrdered 'LittleEndian a)
deriving via ViaPrim (ByteOrdered    'BigEndian a)
    instance (Prim' a, ByteSwap a) => Put (ByteOrdered    'BigEndian a)
