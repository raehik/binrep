{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

{- | Generic sequential sum type serialization.

Here, we use the the provided function to turn the given value's constructor
name into a prefix tag, then pass to the constructor serializer.
-}

module Senserial.Sequential.Serialize.Sum where

import GHC.Generics
import GHC.TypeLits ( TypeError )
import Util.Generic ( conName' )
import Senserial.Internal.Error ( type ENoEmpty, type EUnexpectedNonSum )
import Senserial.Sequential.Serialize.Constructor ( GSeqSerC(gSeqSerC) )

-- | Sequentially serialize a term of the sum type @a@ generically.
seqSerSum
    :: forall bld a
    .  (Generic a, SeqSerSum bld (Rep a))
    => (String -> bld) -> a -> bld
seqSerSum f = gSeqSerDSum f . from

-- | Easier user shorthand for the top-level generic function.
type SeqSerSum = GSeqSerDSum

-- | Generic sum type serializer (data type/top level).
class GSeqSerDSum bld f where
    gSeqSerDSum :: (String -> bld) -> f p -> bld

-- | Unwrap meta (data type/top level).
instance GSeqSerDSum bld f => GSeqSerDSum bld (D1 c f) where
    gSeqSerDSum f (M1 a) = gSeqSerDSum f a

-- | Serialize a term of a sum data type.
instance GSeqSerCSum bld (l :+: r) => GSeqSerDSum bld (l :+: r) where
    gSeqSerDSum = gSeqSerCSum

-- | Refuse to derive a non-sum instance if we expected a sum data type.
instance TypeError EUnexpectedNonSum => GSeqSerDSum bld (C1 c f) where
    gSeqSerDSum = undefined

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GSeqSerDSum bld V1 where
    gSeqSerDSum = undefined

-- | Generic serializer (constructor sum level).
class GSeqSerCSum bld f where gSeqSerCSum :: (String -> bld) -> f p -> bld

-- | Inspect constructor sum tree.
instance (GSeqSerCSum bld l, GSeqSerCSum bld r) => GSeqSerCSum bld (l :+: r) where
    gSeqSerCSum f = \case L1 l -> gSeqSerCSum f l
                          R1 r -> gSeqSerCSum f r

-- | Serialize constructor prefix tag, then constructor contents.
instance (Semigroup bld, Constructor c, GSeqSerC bld f) => GSeqSerCSum bld (C1 c f) where
    gSeqSerCSum serTag (M1 a) = serTag (conName' @c) <> gSeqSerC a
