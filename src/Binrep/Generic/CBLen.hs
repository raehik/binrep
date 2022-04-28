{- | Generically derive 'CBLen' type family instances.

This is mostly playing around -- I've only just learned regular GHC generics,
and pulling everything up to the type level is even more confusing and weird.

A 'CBLen' instance usually indicates a type is either extremely simple or comes
with size information in the type. Be careful deriving or writing instances for
your own types - @BLen@ is usually correct/sufficient.

You can attempt to derive a 'CBLen' type family instance generically for a type
via

    type instance CBLen a = CBLenGeneric w a

As with deriving @BLen@, you must provide the type used to store the sum tag for
sum types.

Then try doing something with it e.g. have GHC derive a @BLen@ instance for you
via the default method (that reifies CBLen)

    deriving anyclass BLen a

Hopefully it either compiles, or you get a useful type error. If not, sorry.
-}

module Binrep.Generic.CBLen where

import Binrep.CBLen
import Binrep.Generic.Internal

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

    GCBLen _ V1         = TypeError GErrRefuseVoid
    GCBLen w (M1 _ _ f) = GCBLen w f

--type family GCBLenSum w (f :: k -> Type) :: Maybe Natural where
type family GCBLenSum w (f :: k -> Type) where
    GCBLenSum w (C1 ('MetaCons name _ _) f)  = JustX (GCBLen w f) name
    GCBLenSum w (l :+: r) = MaybeEq (GCBLenSum w l) (GCBLenSum w r)

type family MaybeEq a b where
    MaybeEq (JustX n nName) (JustX m _) = If (n == m) (JustX n nName) NothingX
    MaybeEq _        _          = NothingX

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
