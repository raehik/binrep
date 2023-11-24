{-# LANGUAGE UndecidableInstances #-} -- for 'CBLenly', 'TypeError'
{-# LANGUAGE AllowAmbiguousTypes #-} -- for 'cblen'

module Binrep.BLen.Ask where

import Raehik.Contravariant.Ask
import Data.Monoid ( Sum(Sum) )
import Data.Functor.Contravariant.Divisible ( divide )

import Binrep.CBLen
import GHC.TypeNats
import Util.TypeNats ( natValInt )

import Data.ByteString qualified as B
import Data.Word
import Data.Int

type Builder = Ask (Sum Int)

blenConst :: Int -> Builder a
blenConst = Ignore . Sum

class BLen a where blen :: Builder a

-- | Deriving via wrapper for types which may derive a 'BLen' instance through
--   an existing 'IsCBLen' instance.
--
-- Examples of such types include machine integers, and explicitly-sized types
-- (e.g. "Binrep.Type.Sized").
newtype CBLenly a = CBLenly { unCBLenly :: a }
instance KnownNat (CBLen a) => BLen (CBLenly a) where
    {-# INLINE blen #-}
    blen = blenConst (cblen @a)

-- | Reify a type's constant byte length to the term level.
cblen :: forall a n. (n ~ CBLen a, KnownNat n) => Int
cblen = natValInt @n
{-# INLINE cblen #-}

{-
instance GenericContra (Ask (Sum Int)) where
    type GenericContraC (Ask (Sum Int)) a = BLen a
    genericContraF = contramap getSum blen

-- | Measure the byte length of a term of the non-sum type @a@ via its 'Generic'
--   instance.
blenGenericNonSum
    :: forall {cd} {f} {asserts} a
    .  ( Generic a, Rep a ~ D1 cd f, GFoldMapNonSum (BLen' Int) f
       , asserts ~ '[ 'NoEmpty, 'NoSum], ApplyGCAsserts asserts f)
    => a -> Int
blenGenericNonSum = getBLen' . genericFoldMapNonSum @asserts

-- | Measure the byte length of a term of the sum type @a@ via its 'Generic'
--   instance.
--
-- You must provide a function to obtain the byte length for the prefix tag, via
-- inspecting the reified constructor names. This is regrettably inefficient.
-- Alas. Do write your own instance if you want better performance!
blenGenericSum
    :: forall {cd} {f} {asserts} a
    .  (Generic a, Rep a ~ D1 cd f, GFoldMapSum 'SumOnly (BLen' Int) f
       , asserts ~ '[ 'NoEmpty, 'NeedSum], ApplyGCAsserts asserts f)
    => (String -> Int) -> a -> Int
blenGenericSum f = getBLen' . genericFoldMapSum @'SumOnly @asserts (BLen' <$> f)

instance TypeError ENoEmpty => BLen Void where blen = undefined
instance TypeError ENoSum => BLen (Either a b) where blen = undefined
-}

-- | Unit type has length 0.
instance BLen () where
    {-# INLINE blen #-}
    blen = blenConst 0

-- | Sum tuples.
instance (BLen l, BLen r) => BLen (l, r) where
    {-# INLINE blen #-}
    blen = divide id blen blen

-- | _O(n)_ Sum the length of each element of a list.
instance BLen a => BLen [a] where
    {-# INLINE blen #-}
    blen = Use $ \as -> case blen @a of
      Ignore (Sum n) -> Sum $ n * length as
      Use    fa      -> foldMap fa as

-- | Length of a bytestring is fairly obvious.
instance BLen B.ByteString where
    {-# INLINE blen #-}
    blen = Use (Sum . B.length)

-- Machine integers have a constant byte length.
deriving via CBLenly Word8  instance BLen Word8
deriving via CBLenly  Int8  instance BLen  Int8
deriving via CBLenly Word16 instance BLen Word16
deriving via CBLenly  Int16 instance BLen  Int16
deriving via CBLenly Word32 instance BLen Word32
deriving via CBLenly  Int32 instance BLen  Int32
deriving via CBLenly Word64 instance BLen Word64
deriving via CBLenly  Int64 instance BLen  Int64
