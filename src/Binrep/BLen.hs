{-# LANGUAGE UndecidableInstances #-} -- for 'ViaCBLen', 'TypeError'
{-# LANGUAGE AllowAmbiguousTypes #-} -- for type-level sum type handling

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
  , blenGenericNonSum, blenGenericSum, blenGenericSumRaw
  , ViaCBLen(..), cblen
  ) where

import Binrep.CBLen
import GHC.TypeNats

import Binrep.Common.Class.TypeErrors ( ENoSum, ENoEmpty )
import GHC.TypeLits ( TypeError )

import Data.Void
import Data.ByteString qualified as B
import Data.Word
import Data.Int
import Binrep.Util.ByteOrder

import Data.Monoid qualified as Monoid
import GHC.Generics
import Generic.Data.Function.FoldMap
import Generic.Data.MetaParse.Cstr ( Raw, ParseCstrTo )
import Generic.Type.Assert
import Binrep.Common.Via.Generically.NonSum

import Rerefined.Refine
import Rerefined.Predicate.Logical.And

-- | Class for types with easily-calculated length in bytes.
--
-- If it appears hard to calculate byte length for a given type (e.g. without
-- first serializing it, then measuring serialized byte length), consider
-- whether this type is a good fit for binrep.
class BLen a where
    -- | Calculate the serialized byte length of the given value.
    blen :: a -> Int

instance GenericFoldMap BLen where
    type GenericFoldMapM BLen = Monoid.Sum Int
    type GenericFoldMapC BLen a = BLen a
    genericFoldMapF = Monoid.Sum . blen

-- | Measure the byte length of a term of the non-sum type @a@ via its 'Generic'
--   instance.
blenGenericNonSum
    :: forall a
    .  ( Generic a, GFoldMapNonSum BLen (Rep a)
       , GAssertNotVoid a, GAssertNotSum a
    ) => a -> Int
blenGenericNonSum = Monoid.getSum . genericFoldMapNonSum @BLen

instance
  ( Generic a, GFoldMapNonSum BLen (Rep a)
  , GAssertNotVoid a, GAssertNotSum a
  ) => BLen (GenericallyNonSum a) where
    blen = blenGenericNonSum . unGenericallyNonSum

-- | Measure the byte length of a term of the sum type @a@ via its 'Generic'
--   instance.
blenGenericSum
    :: forall sumtag a
    .  ( Generic a, GFoldMapSum BLen sumtag (Rep a)
       , GAssertNotVoid a, GAssertSum a
    ) => ParseCstrTo sumtag Int -> a -> Int
blenGenericSum f =
    Monoid.getSum . genericFoldMapSum @BLen @sumtag (\p -> Monoid.Sum (f p))

-- TODO perhaps provide some handy wrappers that fill in blen for sumtag type
-- with cblen? how to do this well?

-- | Measure the byte length of a term of the sum type @a@ via its 'Generic'
--   instance.
blenGenericSumRaw
    :: forall a
    .  ( Generic a, GFoldMapSum BLen Raw (Rep a)
       , GAssertNotVoid a, GAssertSum a
    ) => (String -> Int) -> a -> Int
blenGenericSumRaw f =
    Monoid.getSum . genericFoldMapSumRaw @BLen (Monoid.Sum <$> f)

-- We can't provide a Generically instance because the user must choose between
-- sum and non-sum handlers.

instance BLen (Refined pr (Refined pl a))
  => BLen (Refined (pl `And` pr) a) where
    blen = blen . unsafeRefine @_ @pr . unsafeRefine @_ @pl . unrefine

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

-- | Explicitness does not alter length.
deriving via ViaCBLen (ByteOrdered end a)
    instance KnownNat (CBLen a) => BLen (ByteOrdered end a)

--------------------------------------------------------------------------------

-- | DerivingVia wrapper for types which may derive a 'BLen' instance through
--   an existing 'IsCBLen' instance (i.e. it is known at compile time)
--
-- Examples of such types include machine integers, and explicitly-sized types
-- (e.g. "Binrep.Type.Sized").
newtype ViaCBLen a = ViaCBLen { unViaCBLen :: a }
instance KnownNat (CBLen a) => BLen (ViaCBLen a) where blen _ = cblen @a
