{-# LANGUAGE UndecidableInstances #-} -- due to type class design

{- | Generic sequential field serialization.

Reusable between sum and non-sum types, so we write it once here.
-}

module Senserial.Sequential.Serialize.Internal.Field where

import GHC.Generics
import Senserial.Sequential.Serialize.Internal.Builder ( SeqBuilder(..) )

-- | Generic sequential serializer (constructor level).
class GSeqSerC bld f where gSeqSerC :: f p -> bld

-- | Serialize fields left to right.
instance (Semigroup bld, GSeqSerC bld l, GSeqSerC bld r)
  => GSeqSerC bld (l :*: r) where
    gSeqSerC (l :*: r) = gSeqSerC l <> gSeqSerC r

-- | Serialize a field using the builder's type class.
instance (SeqBuilder bld, SeqBuilderC bld a)
  => GSeqSerC bld (S1 c (Rec0 a)) where
    gSeqSerC (M1 (K1 a)) = seqBuild a

-- | Wow, look! Nothing!
instance Monoid bld => GSeqSerC bld U1 where gSeqSerC U1 = mempty
