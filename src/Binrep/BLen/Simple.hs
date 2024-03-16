{-# LANGUAGE UndecidableInstances #-} -- for 'CBLenly', 'TypeError'
{-# LANGUAGE AllowAmbiguousTypes #-} -- for 'cblen', 'natValInt'

{- | Byte length as a simple pure function, no bells or whistles.

Non-reallocating serializers like store, bytezap or ptr-poker request the
expected total byte length when serializing. Thus, they need some way to measure
byte length *before* serializing. This is that.

It should be very efficient to calculate serialized byte length for most
binrep-compatible Haskell types. If it isn't, consider whether the
representation is appropriate for binrep.
-}

module Binrep.BLen.Simple where

import Binrep.CBLen
import GHC.TypeNats
import Util.TypeNats ( natValInt )

import Binrep.Util.Class
import GHC.TypeLits ( TypeError )

import Data.Void
import Data.ByteString qualified as B
import Data.Word
import Data.Int

import Data.Monoid ( Sum(..) )
import GHC.Generics
import Generic.Data.Function.FoldMap
import Generic.Data.Rep.Assert
import Generic.Data.Function.Common

class BLen a where blen :: a -> Int

-- newtype sum monoid for generic foldMap
newtype BLen' a = BLen' { getBLen' :: a }
    deriving (Semigroup, Monoid) via Sum a

instance GenericFoldMap (BLen' Int) where
    type GenericFoldMapC (BLen' Int) a = BLen a
    genericFoldMapF = BLen' . blen

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

-- | Unit type has length 0.
instance BLen () where
    {-# INLINE blen #-}
    blen () = 0

-- | Sum tuples.
instance (BLen l, BLen r) => BLen (l, r) where
    {-# INLINE blen #-}
    blen (l, r) = blen l + blen r

-- | _O(n)_ Sum the length of each element of a list.
instance BLen a => BLen [a] where
    {-# INLINE blen #-}
    blen = sum . map blen

-- | Length of a bytestring is fairly obvious.
instance BLen B.ByteString where
    {-# INLINE blen #-}
    blen = B.length

-- Machine integers have a constant byte length.
deriving via CBLenly Word8  instance BLen Word8
deriving via CBLenly  Int8  instance BLen  Int8
deriving via CBLenly Word16 instance BLen Word16
deriving via CBLenly  Int16 instance BLen  Int16
deriving via CBLenly Word32 instance BLen Word32
deriving via CBLenly  Int32 instance BLen  Int32
deriving via CBLenly Word64 instance BLen Word64
deriving via CBLenly  Int64 instance BLen  Int64

--------------------------------------------------------------------------------

-- | Deriving via wrapper for types which may derive a 'BLen' instance through
--   an existing 'IsCBLen' instance.
--
-- Examples of such types include machine integers, and explicitly-sized types
-- (e.g. "Binrep.Type.Sized").
newtype CBLenly a = CBLenly { unCBLenly :: a }
instance KnownNat (CBLen a) => BLen (CBLenly a) where
    {-# INLINE blen #-}
    blen _ = cblen @a

-- | Reify a type's constant byte length to the term level.
cblen :: forall a n. (n ~ CBLen a, KnownNat n) => Int
cblen = natValInt @n
{-# INLINE cblen #-}
