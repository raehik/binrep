module Raehik.Validate.Generic.Strengthen where

import Raehik.Validate.Strengthen

import GHC.Generics

strengthenGeneric :: (Generic w, Generic s, GStrengthen (Rep w) (Rep s)) => w -> Either String s
strengthenGeneric = fmap to . gstrengthen . from

class GStrengthen w s where
    gstrengthen :: w p -> Either String (s p)

instance GStrengthen w s => GStrengthen (M1 iw mw w) (M1 is ms s) where
    gstrengthen = fmap M1 . gstrengthen . unM1

instance GStrengthen V1 V1 where
    gstrengthen = Right

instance GStrengthen U1 U1 where
    gstrengthen = Right

instance GStrengthen (Rec0 w) (Rec0 w) where
    gstrengthen = Right

instance {-# OVERLAPS #-} Strengthen w s => GStrengthen (Rec0 w) (Rec0 s) where
    gstrengthen = fmap K1 . strengthen . unK1

instance (GStrengthen lw ls, GStrengthen rw rs) => GStrengthen (lw :*: rw) (ls :*: rs) where
    gstrengthen (l :*: r) = do
        l' <- gstrengthen l
        r' <- gstrengthen r
        return $ l' :*: r'

instance (GStrengthen lw ls, GStrengthen rw rs) => GStrengthen (lw :+: rw) (ls :+: rs) where
    gstrengthen = \case L1 l -> L1 <$> gstrengthen l
                        R1 r -> R1 <$> gstrengthen r
