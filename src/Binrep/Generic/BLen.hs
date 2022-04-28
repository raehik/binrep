module Binrep.Generic.BLen where

import GHC.Generics
import GHC.TypeLits ( TypeError )
import GHC.TypeNats
import GHC.Exts ( proxy#, Proxy# )

import Binrep.BLen
import Binrep.CBLen
import Binrep.Generic.Internal

blenGeneric :: (Generic a, GBLen (Rep a), KnownNat (CBLen w)) => Cfg w -> a -> Natural
blenGeneric cfg = gblen cfg . from

class GBLen f where
    gblen :: KnownNat (CBLen w) => Cfg w -> f p -> Natural

-- | Empty constructor.
instance GBLen U1 where
    gblen _ U1 = 0

-- | Field.
instance BLen c => GBLen (K1 i c) where
    gblen _ (K1 c) = blen c

-- | Product type fields are consecutive.
instance (GBLen l, GBLen r) => GBLen (l :*: r) where
    gblen cfg (l :*: r) = gblen cfg l + gblen cfg r

-- | Constructor sums are differentiated by a prefixing tag byte of constant
--   size. By enforcing constant size, we prevent parsing ambiguity.
instance GBLenSum (l :+: r) => GBLen (l :+: r) where
    gblen :: forall w p. KnownNat (CBLen w) => Cfg w -> (l :+: r) p -> Natural
    gblen cfg x = natVal' (proxy# :: Proxy# (CBLen w)) + gblenSum cfg x

-- | Refuse to derive instance for void datatype.
instance TypeError GErrRefuseVoid => GBLen V1 where
    gblen = undefined

-- | Any datatype, constructor or record.
instance GBLen f => GBLen (M1 i d f) where
    gblen cfg = gblen cfg . unM1

--------------------------------------------------------------------------------

class GBLenSum f where
    gblenSum :: KnownNat (CBLen w) => Cfg w -> f p -> Natural

instance (GBLenSum l, GBLenSum r) => GBLenSum (l :+: r) where
    gblenSum cfg = \case L1 l -> gblenSum cfg l
                         R1 r -> gblenSum cfg r

instance GBLen f => GBLenSum (C1 c f) where
    gblenSum cfg = gblen cfg . unM1
