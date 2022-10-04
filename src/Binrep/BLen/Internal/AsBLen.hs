module Binrep.BLen.Internal.AsBLen where

import GHC.Natural ( minusNaturalMaybe )
import GHC.Num.Natural
import GHC.Exts
import Binrep.Util ( posIntToNat )

-- | Helper definitions for using the given type to store byte lengths.
--
-- Byte lengths must be non-negative. Thus, the ideal representation is a
-- 'Natural'. However, most underlying types that we use ('B.ByteString', lists)
-- store their length in 'Int's. By similarly storing an 'Int' ourselves, we
-- could potentially improve performance.
--
-- I like both options, and don't want to give up either. So we provide helpers
-- via a typeclass so that the user doesn't ever have to think about the
-- underlying type.
--
-- For simplicity, documentation may consider 'a' to be an "unsigned" type. For
-- example, underflow refers to a negative 'a' result.
class AsBLen a where
    -- | Safe blen subtraction, returning 'Nothing' for negative results.
    --
    -- Regular subtraction should only be used when you have a guarantee that it
    -- won't underflow.
    safeBLenSub :: a -> a -> Maybe a

    -- | Convert some 'Int' @i@ where @i >= 0@ to a blen.
    --
    -- This is intended for wrapping the output of 'length' functions.
    posIntToBLen :: Int -> a

    -- | Convert some blen to an 'Int' @i@ where @i >= 0@ to a blen.
    blenToPosInt :: a -> Int

    -- | Convert some 'Word#' @w@ where @w <= maxBound @a@ to a blen.
    wordToBLen# :: Word# -> a

    -- | Convert some 'Natural' @n@ where @n <= maxBound @a@ to a blen.
    natToBLen :: Natural -> a

instance AsBLen Int where
    safeBLenSub x y = if z >= 0 then Just z else Nothing where z = x - y
    {-# INLINE safeBLenSub #-}

    posIntToBLen = id
    {-# INLINE posIntToBLen #-}

    blenToPosInt = id
    {-# INLINE blenToPosInt #-}

    natToBLen = natToInt
    {-# INLINE natToBLen #-}

    wordToBLen# w# = I# (word2Int# w#)
    {-# INLINE wordToBLen# #-}

instance AsBLen Natural where
    safeBLenSub = minusNaturalMaybe
    {-# INLINE safeBLenSub #-}

    posIntToBLen = posIntToNat
    {-# INLINE posIntToBLen #-}

    blenToPosInt = natToInt
    {-# INLINE blenToPosInt #-}

    wordToBLen# = NS
    {-# INLINE wordToBLen# #-}

    natToBLen = id
    {-# INLINE natToBLen #-}

natToInt :: Natural -> Int
natToInt = \case
  NS w# -> wordToBLen# w#
  NB _  -> error "TODO natural too large"
{-# INLINE natToInt #-}
