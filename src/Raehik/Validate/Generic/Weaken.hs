module Raehik.Validate.Generic.Weaken where

import Raehik.Validate.Weaken

import GHC.Generics

weakenGeneric :: (Generic s, Generic w, GWeaken (Rep s) (Rep w)) => s -> w
weakenGeneric = to . gweaken . from

class GWeaken s w where
    gweaken :: s p -> w p

instance GWeaken s w => GWeaken (M1 is ms s) (M1 iw mw w) where
    gweaken = M1 . gweaken . unM1

instance GWeaken V1 V1 where
    gweaken = id

instance GWeaken U1 U1 where
    gweaken = id

instance GWeaken (Rec0 s) (Rec0 s) where
    gweaken = id

instance {-# OVERLAPS #-} Weaken s w => GWeaken (Rec0 s) (Rec0 w) where
    gweaken = K1 . weaken . unK1

instance (GWeaken ls lw, GWeaken rs rw) => GWeaken (ls :*: rs) (lw :*: rw) where
    gweaken (l :*: r) = gweaken l :*: gweaken r

instance (GWeaken ls lw, GWeaken rs rw) => GWeaken (ls :+: rs) (lw :+: rw) where
    gweaken = \case L1 l -> L1 $ gweaken l
                    R1 r -> R1 $ gweaken r
