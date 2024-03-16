{-# LANGUAGE UndecidableInstances #-} -- for 'CBLenly', 'TypeError'
{-# LANGUAGE AllowAmbiguousTypes #-} -- for 'cblen', 'natValInt'

{- | Byte length as a simple pure function, no bells or whistles.

Non-reallocating serializers like store, bytezap or ptr-poker request the
expected total byte length when serializing. Thus, they need some way to measure
byte length *before* serializing. This is that.

It should be very efficient to calculate serialized byte length for most
binrep-compatible Haskell types. If it isn't, consider whether the
representation is appropriate for binrep.

Note that you _may_ encode this inside the serializer type (whatever the @Put@
class stores). I went back and forth on this a couple times. But some binrep
code seems to make more sense when byte length is standalone. And I don't mind
the extra explicitness. So it's here to stay :)
-}

module Binrep.BLen
  ( BLen(blen)
  , blenGenericNonSum, blenGenericSum
  , ViaCBLen(..), cblen
  ) where

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

-- | Class for types with easily-calculated length in bytes.
--
-- If it appears hard to calculate byte length for a given type (e.g. without
-- first serializing it, then measuring serialized byte length), consider
-- whether this type is a good fit for binrep.
class BLen a where
    -- | Calculate the serialized byte length of the given value.
    blen :: a -> Int

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

-- | _O(1)_ Unit type has length 0.
instance BLen () where blen () = 0

-- | _O(1)_ Sum tuples.
instance (BLen l, BLen r) => BLen (l, r) where blen (l, r) = blen l + blen r

-- | _O(n)_ Sum the length of each element of a list.
instance BLen a => BLen [a] where blen = sum . map blen

-- | _O(1)_ 'B.ByteString's store their own length.
instance BLen B.ByteString where blen = B.length

-- All words have a constant byte length-- including host-size words, mind you!
deriving via ViaCBLen Word8  instance BLen Word8
deriving via ViaCBLen  Int8  instance BLen  Int8
deriving via ViaCBLen Word16 instance BLen Word16
deriving via ViaCBLen  Int16 instance BLen  Int16
deriving via ViaCBLen Word32 instance BLen Word32
deriving via ViaCBLen  Int32 instance BLen  Int32
deriving via ViaCBLen Word64 instance BLen Word64
deriving via ViaCBLen  Int64 instance BLen  Int64

--------------------------------------------------------------------------------

-- | DerivingVia wrapper for types which may derive a 'BLen' instance through
--   an existing 'IsCBLen' instance (i.e. it is known at compile time)
--
-- Examples of such types include machine integers, and explicitly-sized types
-- (e.g. "Binrep.Type.Sized").
newtype ViaCBLen a = ViaCBLen { unCBLenly :: a }
instance KnownNat (CBLen a) => BLen (ViaCBLen a) where blen _ = cblen @a

-- | Reify a type's constant byte length to the term level.
cblen :: forall a n. (n ~ CBLen a, KnownNat n) => Int
cblen = natValInt @n
