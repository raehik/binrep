{-# LANGUAGE UndecidableInstances #-} -- due to various type algebra
{-# LANGUAGE AllowAmbiguousTypes  #-} -- for reification util

module Binrep.CBLen where

import GHC.TypeNats
import Data.Word
import Data.Int
import Binrep.Util.ByteOrder

import GHC.Exts ( Int#, Int(I#), Proxy# )
import Util.TypeNats ( natValInt )

import DeFun.Core ( type (~>), type App )

import Rerefined.Refine
import Rerefined.Predicate.Logical.And

import Binrep.Common.Class.TypeErrors ( ENoEmpty )

import GHC.Generics
import GHC.TypeError
import Data.Kind ( type Type )

import Data.Type.Equality
import Data.Type.Bool

import Bytezap.Common.Generic ( type GTFoldMapCAddition )

import Binrep.Common.Via.Generically.NonSum

import Strongweak.WeakenN

class IsCBLen a where type CBLen a :: Natural

-- | Deriving via this instance necessitates @UndecidableInstances@.
instance Generic a => IsCBLen (GenericallyNonSum a) where
    type CBLen (GenericallyNonSum a) = CBLenGenericNonSum a

instance IsCBLen (Refined pr (Refined pl a))
  =>   IsCBLen (Refined (pl `And` pr) a) where
    type CBLen (Refined (pl `And` pr) a) = CBLen (Refined pr (Refined pl a))

instance (IsCBLen l, IsCBLen r) => IsCBLen (l, r) where
    type CBLen (l, r) = CBLen l + CBLen r

instance IsCBLen () where type CBLen () = 0

instance IsCBLen Word8  where type CBLen Word8  = 2^0
instance IsCBLen  Int8  where type CBLen  Int8  = 2^0
instance IsCBLen Word16 where type CBLen Word16 = 2^1
instance IsCBLen  Int16 where type CBLen  Int16 = 2^1
instance IsCBLen Word32 where type CBLen Word32 = 2^2
instance IsCBLen  Int32 where type CBLen  Int32 = 2^2
instance IsCBLen Word64 where type CBLen Word64 = 2^3
instance IsCBLen  Int64 where type CBLen  Int64 = 2^3

instance IsCBLen a => IsCBLen (ByteOrdered end a) where
    type CBLen (ByteOrdered end a) = CBLen a

-- | Unwrap strongweak wrapper.
instance IsCBLen a => IsCBLen (WeakenN n a) where
    type CBLen (WeakenN n a) = CBLen a

-- | Reify a type's constant byte length to the term level.
cblen :: forall a. KnownNat (CBLen a) => Int
cblen = natValInt @(CBLen a)

cblen# :: forall a. KnownNat (CBLen a) => Int#
cblen# = i#
  where !(I# i#) = natValInt @(CBLen a)

cblenProxy# :: forall a. KnownNat (CBLen a) => Proxy# a -> Int#
cblenProxy# _ = i#
  where !(I# i#) = natValInt @(CBLen a)

-- | Defunctionalization symbol for 'CBLen'.
--
-- This is required for parameterized type-level generics e.g. bytezap's
-- 'Bytezap.Struct.Generic.GPokeBase'.
type CBLenSym :: a ~> Natural
data CBLenSym a
type instance App CBLenSym a = CBLen a

{- $generic-cblen

Generically derive 'CBLen' type family instances.

A type having a valid 'CBLen' instance usually indicates one of the following:

  * it's a primitive, or extremely simple
  * it holds size information in its type
  * it's constructed from other constant byte length types

The first two cases must be handled manually. The third case is where Haskell
generics excel, and the one this module targets.

You may derive a 'CBLen' type generically for a non-sum type with

    instance IsCBLen a where type CBLen a = CBLenGenericNonSum a

You may attempt to derive a 'CBLen' type generically for a sum type with

    instance IsCBLen a where type CBLen a = CBLenGenericSum w a

As with other generic sum type handlers, you must provide the type used to store
the sum tag for sum types. That sum tag type must have a 'CBLen', and every
constructor must have the same 'CBLen' for a 'CBLen' to be calculated. Not many types will fit those criteria, and the code is not well-tested.
-}

-- | Using this necessitates @UndecidableInstances@.
type CBLenGenericSum (w :: Type) a = GCBLen w (Rep a)

-- | Using this necessitates @UndecidableInstances@.
type CBLenGenericNonSum a = GTFoldMapCAddition CBLenSym (Rep a)

type family GCBLen w (gf :: k -> Type) :: Natural where
    GCBLen w (D1 _ gf) = GCBLen w gf
    GCBLen _ V1        = TypeError ENoEmpty
    GCBLen w (l :+: r) = CBLen w + GCBLenCaseMaybe (GCBLenSum (l :+: r))
    GCBLen w (C1 _ gf) = GTFoldMapCAddition CBLenSym gf

--type family GCBLenSum (gf :: k -> Type) :: Maybe Natural where
type family GCBLenSum (gf :: k -> Type) where
    GCBLenSum (C1 ('MetaCons name _ _) gf) =
        JustX (GTFoldMapCAddition CBLenSym gf) name
    GCBLenSum (l :+: r) = MaybeEq (GCBLenSum l) (GCBLenSum r)

type family MaybeEq a b where
    MaybeEq (JustX n nName) (JustX m _) = If (n == m) (JustX n nName) NothingX
    MaybeEq _               _           = NothingX

-- | I don't know how to pattern match in types without writing type families.
type family GCBLenCaseMaybe a where
    GCBLenCaseMaybe (JustX n _) = n
    GCBLenCaseMaybe NothingX  =
        TypeError
            (     'Text "Two constructors didn't have equal constant size."
            ':$$: 'Text "Sry dunno how to thread errors thru LOL"
            )

-- TODO rewrite this stuff to thread error info through!
data JustX a b
data NothingX
