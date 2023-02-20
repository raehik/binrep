{-# LANGUAGE UndecidableInstances #-} -- hugely unsafe module

{- | _Experimental._ Generically derive 'CBLen' type family instances.

A type having a valid 'CBLen' instance usually indicates one of the following:

  * it's a primitive, or extremely simple
  * it holds size information in its type
  * it's constructed from other constant byte length types

The first two cases must be handled manually. The third case is where Haskell
generics excel, and the one this module targets.

You can (attempt to) derive a 'CBLen' type family instance generically for a
type via

    instance BLen a where type CBLen a = CBLenGeneric w a

As with deriving @BLen@ generically, you must provide the type used to store the
sum tag for sum types.

Then try using it. Hopefully it works, or you get a useful type error. If not,
sorry. I don't have much faith in this code.
-}

module Binrep.CBLen.Generic where

import Binrep.CBLen
import Binrep.Util.Class

import GHC.Generics
import GHC.TypeLits
import Data.Kind

import Data.Type.Equality
import Data.Type.Bool

type CBLenGeneric w a = GCBLen w (Rep a)

type family GCBLen w (f :: k -> Type) :: Natural where
    GCBLen _ U1         = 0
    GCBLen _ (K1 i c)   = CBLen c
    GCBLen w (l :*: r)  = GCBLen w l + GCBLen w r

    GCBLen w (l :+: r)  = CBLen w + GCBLenCaseMaybe (GCBLenSum w (l :+: r))

    GCBLen _ V1         = TypeError ENoEmpty
    GCBLen w (M1 _ _ f) = GCBLen w f

--type family GCBLenSum w (f :: k -> Type) :: Maybe Natural where
type family GCBLenSum w (f :: k -> Type) where
    GCBLenSum w (C1 ('MetaCons name _ _) f)  = JustX (GCBLen w f) name
    GCBLenSum w (l :+: r) = MaybeEq (GCBLenSum w l) (GCBLenSum w r)

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

