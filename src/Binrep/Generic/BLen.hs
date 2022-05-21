module Binrep.Generic.BLen where

import GHC.Generics
import GHC.TypeLits ( TypeError )

import Binrep.BLen
import Binrep.Generic.Internal

blenGeneric :: (Generic a, GBLen (Rep a), BLen w) => Cfg w -> a -> BLenT
blenGeneric cfg = gblen cfg . from

class GBLen f where
    gblen :: BLen w => Cfg w -> f p -> BLenT

-- | Empty constructor.
instance GBLen U1 where
    gblen _ U1 = 0

-- | Field.
instance BLen c => GBLen (K1 i c) where
    gblen _ (K1 c) = blen c

-- | Product type fields are consecutive.
instance (GBLen l, GBLen r) => GBLen (l :*: r) where
    gblen cfg (l :*: r) = gblen cfg l + gblen cfg r

-- | Constructor sums are differentiated by a prefix tag.
instance GBLenSum (l :+: r) => GBLen (l :+: r) where
    gblen = gblensum

-- | Refuse to derive instance for void datatype.
instance TypeError GErrRefuseVoid => GBLen V1 where
    gblen = undefined

-- | Any datatype, constructor or record.
instance GBLen f => GBLen (M1 i d f) where
    gblen cfg = gblen cfg . unM1

--------------------------------------------------------------------------------

class GBLenSum f where
    gblensum :: BLen w => Cfg w -> f p -> BLenT

instance (GBLenSum l, GBLenSum r) => GBLenSum (l :+: r) where
    gblensum cfg = \case L1 l -> gblensum cfg l
                         R1 r -> gblensum cfg r

instance (GBLen f, Constructor c) => GBLenSum (C1 c f) where
    gblensum cfg x = blen ((cSumTag cfg) (conName' @c)) + gblen cfg (unM1 x)
