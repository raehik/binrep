module Senserial.Sequential.Sum where

import GHC.Generics
import GHC.TypeError ( TypeError )
import Util.Generic ( conName' )
import Senserial.Internal.Error ( type ENoEmpty, type EUnexpectedNonSum )
import Senserial.Sequential.Internal.Field ( GSeqSerC(gSeqSerC) )
import Senserial.Sequential.Internal.Builder ( SeqBuilder)

-- | Sequentially serialize a term of the sum type @a@ generically.
seqSerSum
    :: forall bld a
    .  (Generic a, GSeqSerDSum bld (Rep a))
    => (String -> bld) -> a -> bld
seqSerSum f = gSeqSerDSum f . from

-- | Easier user shorthand for the top-level serializer.
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
instance (SeqBuilder bld, Constructor c, GSeqSerC bld f) => GSeqSerCSum bld (C1 c f) where
    gSeqSerCSum serTag (M1 a) = serTag (conName' @c) <> gSeqSerC a
