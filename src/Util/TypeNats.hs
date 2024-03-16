{-# LANGUAGE AllowAmbiguousTypes #-} -- for my TypeApplications-based natVals
{-# LANGUAGE UndecidableInstances #-} -- for Length type family

-- | Handy typenat utils.

module Util.TypeNats where

-- natVal''
import GHC.TypeNats ( Natural, KnownNat, natVal', type (+) )
import GHC.Exts ( proxy#, Proxy# )

natVal'' :: forall n. KnownNat n => Natural
natVal'' = natVal' (proxy# :: Proxy# n)
{-# INLINE natVal'' #-}

natValInt :: forall n. KnownNat n => Int
natValInt = fromIntegral $ natVal'' @n
{-# INLINE natValInt #-}

-- TODO might wanna move this
-- | The length of a type-level list.
type family Length (a :: [k]) :: Natural where
    Length '[]       = 0
    Length (a ': as) = 1 + Length as
