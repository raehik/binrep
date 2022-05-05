module Binrep.Generic.Put where

import GHC.Generics
import GHC.TypeLits ( TypeError )

import Binrep.Put
import Binrep.Generic.Internal

putGeneric :: (Generic a, GPut (Rep a), Put w) => Cfg w -> a -> Builder
putGeneric cfg = gput cfg . from

class GPut f where
    gput :: Put w => Cfg w -> f p -> Builder

-- | Empty constructor.
instance GPut U1 where
    gput _ U1 = mempty

-- | Field.
instance Put c => GPut (K1 i c) where
    gput _ = put . unK1

-- | Product type fields are consecutive.
instance (GPut l, GPut r) => GPut (l :*: r) where
    gput cfg (l :*: r) = gput cfg l <> gput cfg r

-- | Constructor sums are differentiated by a prefix tag.
instance (GPutSum (l :+: r), GetConName (l :+: r)) => GPut (l :+: r) where
    gput = gputsum

-- | Refuse to derive instance for void datatype.
instance TypeError GErrRefuseVoid => GPut V1 where
    gput = undefined

-- | Any datatype, constructor or record.
instance GPut f => GPut (M1 i d f) where
    gput cfg = gput cfg . unM1

--------------------------------------------------------------------------------

class GPutSum f where
    gputsum :: Put w => Cfg w -> f a -> Builder

instance (GPutSum l, GPutSum r) => GPutSum (l :+: r) where
    gputsum cfg = \case L1 a -> gputsum cfg a
                        R1 a -> gputsum cfg a

instance (GPut r, Constructor c) => GPutSum (C1 c r) where
    gputsum cfg x = putTag <> putConstructor
      where putTag = put $ (cSumTag cfg) (conName' @c)
            putConstructor = gput cfg $ unM1 x

---

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance Constructor c => GetConName (C1 c a) where
    getConName = conName
