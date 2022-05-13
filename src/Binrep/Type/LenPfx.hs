-- TODO cleanup proxy usage (can we be faster via unboxed @Proxy#@ s ?)

{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.LenPfx where

import Binrep
import Binrep.Type.Vector()
import Binrep.Type.Common ( Endianness )
import Binrep.Type.Int
import Binrep.Util ( natVal'' )
import Data.Vector.Sized ( Vector )
import GHC.TypeNats
import GHC.TypeLits ( OrderingI(..) )
import Data.Proxy ( Proxy(..) )

-- | Holy shit - no need to do a smart constructor, it's simply impossible to
--   instantiate invalid values of this type!
data LenPfx (size :: ISize) (end :: Endianness) a =
    forall n. (KnownNat n, n <= IMax 'U size) => LenPfx { unLenPfx :: Vector n a }

asLenPfx
    :: forall size end n a irep
    .  (irep ~ IRep 'U size, KnownNat n, KnownNat (MaxBound irep))
    => Vector n a -> Maybe (LenPfx size end a)
asLenPfx v =
    case cmpNat (Proxy :: Proxy n) (Proxy :: Proxy (MaxBound (IRep 'U size))) of
      LTI -> Just $ LenPfx v
      EQI -> Just $ LenPfx v
      GTI -> Nothing

instance (BLen a, itype ~ I 'U size end, KnownNat (CBLen itype))
  => BLen (LenPfx size end a) where
    blen (LenPfx v) = cblen @itype + blen v

instance (itype ~ I 'U size end, irep ~ IRep 'U size, Put a, Put itype, Num irep)
  => Put (LenPfx size end a) where
    put (LenPfx v) = put @itype (fromIntegral (vnatVal v)) <> put v
      where
        vnatVal :: forall n x. KnownNat n => Vector n x -> Natural
        vnatVal _ = natVal'' @n

instance (itype ~ I 'U size end, irep ~ IRep 'U size, Get itype, Integral irep, Get a, KnownNat (MaxBound irep))
  => Get (LenPfx size end a) where
    get = do
        len <- get @itype
        case someNatVal (fromIntegral len) of
          SomeNat (Proxy :: Proxy n) -> do
            x <- get @(Vector n a)
            -- TODO we actually know that @n <= MaxBound irep@ before doing this
            -- because @len <= maxBound (_ :: irep)@ but that's hard to prove to
            -- GHC without lots of refactoring. This is good enough.
            case cmpNat (Proxy :: Proxy n) (Proxy :: Proxy (MaxBound irep)) of
              GTI -> error "impossible"
              LTI -> return $ LenPfx x
              EQI -> return $ LenPfx x
