module Binrep.Generic.Put where

import GHC.Generics
import GHC.TypeLits ( TypeError )

import Binrep.Put
import Binrep.Generic.Internal

import Data.Serialize.Put qualified as Cereal

putGeneric :: (Generic a, GPut (Rep a), Put w) => Cfg w -> Cereal.Putter a
putGeneric cfg = gput cfg . from

class GPut f where
    gput :: Put w => Cfg w -> Cereal.Putter (f p)

-- | Empty constructor.
instance GPut U1 where
    gput _ U1 = return ()

-- | Field.
instance Put c => GPut (K1 i c) where
    gput _ = put . unK1

-- | Product type fields are consecutive.
instance (GPut l, GPut r) => GPut (l :*: r) where
    gput cfg (l :*: r) = gput cfg l *> gput cfg r

-- | Constructor sums are differentiated by a prefixing tag byte of constant
--   size. By enforcing constant size, we prevent parsing ambiguity.
instance (GPutSum (l :+: r), GetConName (l :+: r)) => GPut (l :+: r) where
    gput cfg a = do
        put $ cSumTag cfg $ getConName a
        gputSum cfg a

-- | Refuse to derive instance for void datatype.
instance TypeError GErrRefuseVoid => GPut V1 where
    gput = undefined

-- | Any datatype, constructor or record.
instance GPut f => GPut (M1 i d f) where
    gput cfg = gput cfg . unM1

--------------------------------------------------------------------------------

class GPutSum f where
    gputSum :: Put w => Cfg w -> Cereal.Putter (f a)

instance (GPutSum l, GPutSum r) => GPutSum (l :+: r) where
    gputSum cfg = \case L1 a -> gputSum cfg a
                        R1 a -> gputSum cfg a

instance (GPut r, Constructor c) => GPutSum (C1 c r) where
    gputSum cfg = gput cfg . unM1

---

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance Constructor c => GetConName (C1 c a) where
    getConName = conName
