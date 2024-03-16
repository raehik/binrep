{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for generic data op instance

{- | Serialization using the bytezap library.

bytezap serializers ("pokes") work by writing bytes into a pointer, which is
assumed to have _precisely_ the space required. The user must determine the
post-serialize length before the fact. For that reason, this module requires
that types to be serialized have a 'BLen' instance. In general, we are happy
about this, because a binrep type should always have an efficient and preferably
simple 'BLen' instance (and if not, it shouldn't be a binrep type).
-}

module Binrep.Put where

import Bytezap.Write
import Raehik.Compat.Data.Primitive.Types ( Prim' )
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteOrdered(..), ByteSwap )
import GHC.ByteOrder

import Data.ByteString qualified as B

import Binrep.Util.Class
import GHC.TypeLits ( TypeError )

import Data.Void
import Data.Word
import Data.Int

import GHC.Generics
import Generic.Data.Function.FoldMap
import Generic.Data.Function.Common
import Generic.Data.Rep.Assert

import Control.Monad.ST ( RealWorld )

type Write' = Write RealWorld

class Put a where put :: a -> Write'

runPut :: Put a => a -> B.ByteString
runPut = runWriteBS . put

instance GenericFoldMap Write' where
    type GenericFoldMapC Write' a = Put a
    genericFoldMapF = put

-- | Serialize a term of the non-sum type @a@ via its 'Generic' instance.
putGenericNonSum
    :: forall {cd} {f} {asserts} a
    .  ( Generic a, Rep a ~ D1 cd f, GFoldMapNonSum Write' f
       , asserts ~ '[ 'NoEmpty, 'NoSum], ApplyGCAsserts asserts f)
    => a -> Write'
putGenericNonSum = genericFoldMapNonSum @asserts

-- | Serialize a term of the sum type @a@ via its 'Generic' instance.
--
-- You must provide a serializer for @a@'s constructors. This is regrettably
-- inefficient due to having to use 'String's. Alas. Do write your own instance
-- if you want better performance!
putGenericSum
    :: forall {cd} {f} {asserts} a
    .  (Generic a, Rep a ~ D1 cd f, GFoldMapSum 'SumOnly Write' f
       , asserts ~ '[ 'NoEmpty, 'NeedSum], ApplyGCAsserts asserts f)
    => (String -> Write') -> a -> Write'
putGenericSum = genericFoldMapSum @'SumOnly @asserts

-- | DerivingVia newtype for 'Put' types which can borrow from 'Prim''.
newtype PutViaPrim a = PutViaPrim { unPutViaPrim :: a }
instance Prim' a => Put (PutViaPrim a) where put = prim . unPutViaPrim

instance TypeError ENoEmpty => Put Void where put = undefined
instance TypeError ENoSum => Put (Either a b) where put = undefined

instance Put Write' where put = id

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
deriving via PutViaPrim Word8 instance Put Word8

-- | 8-bit (1-byte) words do not require byte order in order to precisely
--   define their representation.
deriving via PutViaPrim  Int8 instance Put  Int8

-- TODO maybe via binreply for these two (but need to move instances then...?)
-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via PutViaPrim Word8 instance Put  (ByteOrdered end Word8)

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via PutViaPrim  Int8 instance Put  (ByteOrdered end  Int8)

-- ByteSwap is required on opposite endian platforms, but we're not checking
-- here, so make sure to keep it on both.
deriving via PutViaPrim (ByteOrdered 'LittleEndian a)
    instance (Prim' a, ByteSwap a) => Put (ByteOrdered 'LittleEndian a)
deriving via PutViaPrim (ByteOrdered    'BigEndian a)
    instance (Prim' a, ByteSwap a) => Put (ByteOrdered    'BigEndian a)
