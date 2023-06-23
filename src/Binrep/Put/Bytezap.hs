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

module Binrep.Put.Bytezap where

import Bytezap
import Bytezap.Poke.Bytes
import Bytezap.Poke.Int
import Data.ByteString qualified as B
import Binrep.BLen.Simple

import Binrep.Util.Class
import GHC.TypeLits ( TypeError )

import Data.Void
import Data.Word
import Data.Int

import GHC.Generics ( Generic, type Rep )
import Generic.Data.Function.FoldMap

class Put a where put :: a -> Poke

runPut :: (BLen a, Put a) => a -> B.ByteString
runPut a = runPoke (blen a) (put a)
{-# INLINE runPut #-}

instance GenericFoldMap Poke where
    type GenericFoldMapC Poke a = Put a
    genericFoldMapF = put

-- | Serialize a term of the non-sum type @a@ via its 'Generic' instance.
putGenericNonSum
    :: (Generic a, GFoldMapNonSum Poke (Rep a))
    => a -> Poke
putGenericNonSum = genericFoldMapNonSum

-- | Serialize a term of the sum type @a@ via its 'Generic' instance.
--
-- You must provide a serializer for @a@'s constructors. This is regrettably
-- inefficient due to having to use 'String's. Alas. Do write your own instance
-- if you want better performance!
putGenericSum
    :: (Generic a, GFoldMapSum Poke (Rep a))
    => (String -> Poke) -> a -> Poke
putGenericSum = genericFoldMapSum

instance TypeError ENoEmpty => Put Void where put = undefined
instance TypeError ENoSum => Put (Either a b) where put = undefined

instance Put Write where
    {-# INLINE put #-}
    put = writePoke

-- | Fairly useless because 'Poke' doesn't have a 'BLen' instance.
instance Put Poke where
    {-# INLINE put #-}
    put = id

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

instance Put Word8 where
    {-# INLINE put #-}
    put = w8

instance Put Int8  where
    {-# INLINE put #-}
    put = i8
