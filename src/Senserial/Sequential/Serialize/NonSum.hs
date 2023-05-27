{- | Generic sequential non-sum type serialization.

All we do here is unwrap, then pass to the constructor serializer.
-}

module Senserial.Sequential.Serialize.NonSum where

import GHC.Generics
import GHC.TypeError ( TypeError )
import Senserial.Internal.Error ( type ENoEmpty, type EUnexpectedSum )
import Senserial.Sequential.Serialize.Constructor ( GSeqSerC(gSeqSerC) )

-- | Sequentially serialize a term of the non-sum type @a@ generically.
seqSerNonSum
    :: forall bld a
    .  (Generic a, SeqSerNonSum bld (Rep a))
    => a -> bld
seqSerNonSum = gSeqSerDNonSum . from

-- | Easier user shorthand for the top-level generic function.
type SeqSerNonSum = GSeqSerDNonSum

-- | Generic non-sum type serializer (data type/top level).
class GSeqSerDNonSum bld f where gSeqSerDNonSum :: f p -> bld

-- | Unwrap meta (data type/top level).
instance GSeqSerDNonSum bld f => GSeqSerDNonSum bld (D1 c f) where
    gSeqSerDNonSum (M1 a) = gSeqSerDNonSum a

-- | Refuse to derive a sum instance if we expected a non-sum data type.
instance TypeError EUnexpectedSum => GSeqSerDNonSum bld (l :+: r) where
    gSeqSerDNonSum = undefined

-- | Serialize the single constructor of a non-sum data type.
instance GSeqSerC bld f => GSeqSerDNonSum bld (C1 c f) where
    gSeqSerDNonSum (M1 a) = gSeqSerC a

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GSeqSerDNonSum bld V1 where
    gSeqSerDNonSum = undefined
