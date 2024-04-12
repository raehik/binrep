{-# LANGUAGE AllowAmbiguousTypes #-} -- for my TypeApplications-based natVals

-- | Handy typenat utils.

module Util.TypeNats where

-- natVal''
import GHC.TypeNats ( Natural, KnownNat, natVal' )
import GHC.Exts ( proxy#, Proxy# )

natVal'' :: forall n. KnownNat n => Natural
natVal'' = natVal' (proxy# :: Proxy# n)
{-# INLINE natVal'' #-}

natValInt :: forall n. KnownNat n => Int
natValInt = fromIntegral $ natVal'' @n
{-# INLINE natValInt #-}
