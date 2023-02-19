{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- required for easier instances
{-# LANGUAGE OverloadedStrings #-} -- for refined errors

{- TODO
  * Max () = 1?
-}

module Binrep.Type.LengthPrefixed where

import Binrep
import FlatParse.Basic qualified as FP

import Binrep.Type.Int
import Data.Vector.Sized ( Vector )
import Data.Vector.Sized qualified as V
import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Data.Word
import Data.ByteString qualified as B
import Refined hiding ( Weaken )
import Refined.Unsafe

import Data.Typeable ( Typeable, typeRep )
import Data.List qualified as List

class HasLength a where getLength :: a -> Int
instance KnownNat n => HasLength (Vector n a) where getLength = V.length
instance HasLength B.ByteString where getLength = B.length
instance HasLength [a] where getLength = List.length

class HasLength a => KnownLength a where type Length a :: Natural
instance KnownNat n => KnownLength (Vector n a) where
    type Length (Vector n a) = n

data LengthPrefix pfx
type LengthPrefixed pfx = Refined (LengthPrefix pfx)

instance (HasLength a, KnownNat (Max pfx), Typeable pfx)
  => Predicate (LengthPrefix pfx) a where
    validate p a
      | getLength a <= natValInt @(Max pfx) = Nothing
      | otherwise = throwRefineOtherException (typeRep p) $
          "thing too big for length prefix type"

-- compile time check
lengthPrefix
    :: forall pfx a
    .  (Length a <= Max pfx)
    => a -> LengthPrefixed pfx a
lengthPrefix = reallyUnsafeRefine

-- TODO no idea if this is sensible
instance IsCBLen (LengthPrefixed pfx a) where
    type CBLen (LengthPrefixed pfx a) = CBLen pfx + CBLen a

instance (Prefix pfx, HasLength a, BLen pfx, BLen a)
  => BLen (LengthPrefixed pfx a) where
    blen ra = blen (lenToPfx @pfx (getLength a)) + blen a
      where a = unrefine ra

instance (Prefix pfx, HasLength a, Put pfx, Put a)
  => Put (LengthPrefixed pfx a) where
    put ra = put (lenToPfx @pfx (getLength a)) <> put a
      where a = unrefine ra

instance (Prefix pfx, Get pfx, Get a, GetLength a)
  => Get (LengthPrefixed pfx a) where
    get = do
        pfx <- get @pfx
        a <- getLength' (pfxToLen pfx)
        pure $ reallyUnsafeRefine a

-- TODO getcount instead? idk
class GetLength a where getLength' :: Int -> Getter a
instance GetLength B.ByteString where getLength' = FP.take

class Prefix a where
    type Max a :: Natural

    -- | used by put. guaranteed that it fits from refined.
    lenToPfx :: Int -> a

    -- | used by get. better not lie.
    pfxToLen :: a -> Int

-- | Length prefixing with the unit means a length of 0.
--
-- This is the only sensible case. 1 doesn't work because refining checks @<=@.
--
-- I think there are laws here, where using this is the same as doing nothing at
-- all.
instance Prefix () where
    type Max () = 0
    lenToPfx 0 = ()
    lenToPfx _ = error "() prefix somehow had non-empty data?? you fucked up."
    pfxToLen () = 0

-- I don't think @'Prefix' 'Void'@ is a sensible instance.

instance Prefix a => Prefix (Endian end a) where
    type Max (Endian end a) = Max a
    lenToPfx = Endian . lenToPfx
    pfxToLen = pfxToLen . unEndian
instance Prefix Word8  where
    type Max Word8  = 2^8  - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
instance Prefix Word16 where
    type Max Word16 = 2^16 - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
instance Prefix Word32 where
    type Max Word32 = 2^32 - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
instance Prefix Word64 where
    type Max Word64 = 2^64 - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
