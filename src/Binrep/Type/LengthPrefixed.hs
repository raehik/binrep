{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- required for easier instances
{-# LANGUAGE OverloadedStrings #-} -- for refined errors

{- TODO 2023-02-20
  * split into sizeprefix (Type) and lengthprefix (Type -> Type)
  * better behaviour for lengthprefix, sizeprefix is a special case
-}

module Binrep.Type.LengthPrefixed where

import Binrep
import FlatParse.Basic qualified as FP
import Control.Monad.Combinators qualified as Monad

import Binrep.Type.Int
import Data.Vector.Sized ( Vector )
import Data.Vector.Sized qualified as V
import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Data.Word
import Data.ByteString qualified as B
import Refined hiding ( Weaken(..), strengthen )
import Refined.Unsafe
import Refined.Unsafe.Type

import Data.Typeable ( Typeable, typeRep )
import Data.List qualified as List

import Data.Foldable qualified as Foldable

import Strongweak

data CountPrefix pfx
type CountPrefixed pfx = Refined (CountPrefix pfx)

instance (KnownNat (Max pfx), Foldable f, Typeable pfx)
  => Predicate (CountPrefix pfx) (f a) where
    validate p a
      | Foldable.length a <= natValInt @(Max pfx) = Nothing
      | otherwise = throwRefineOtherException (typeRep p) $
          "thing too big for length prefix type"

-- TODO no idea if this is sensible
instance IsCBLen (CountPrefixed pfx a) where
    type CBLen (CountPrefixed pfx a) = CBLen pfx + CBLen a

instance (Prefix pfx, Foldable f, BLen pfx, BLen (f a))
  => BLen (CountPrefixed pfx (f a)) where
    blen ra = blen (lenToPfx @pfx (Foldable.length a)) + blen a
      where a = unrefine ra

instance (Prefix pfx, Foldable f, Put pfx, Put (f a))
  => Put (CountPrefixed pfx (f a)) where
    put ra = put (lenToPfx @pfx (Foldable.length a)) <> put a
      where a = unrefine ra

-- Fucking lol dude this is wicked
class GetF f where getFCount :: Get a => Int -> Getter (f a)
instance GetF [] where getFCount n = Monad.count n get

-- Suck my nuts
instance (Prefix pfx, GetF f, Get pfx, Get a)
  => Get (CountPrefixed pfx (f a)) where
    get = do
        pfx <- get @pfx
        a <- getFCount (pfxToLen pfx)
        pure $ reallyUnsafeRefine a

data SizePrefix pfx
type SizePrefixed pfx = Refined (SizePrefix pfx)

instance (KnownNat (Max pfx), BLen a, Typeable pfx)
  => Predicate (SizePrefix pfx) a where
    validate p a
      | blen a <= natValInt @(Max pfx) = Nothing
      | otherwise = throwRefineOtherException (typeRep p) $
          "thing too big for length prefix type"

-- TODO no idea if this is sensible
instance IsCBLen (SizePrefixed pfx a) where
    type CBLen (SizePrefixed pfx a) = CBLen pfx + CBLen a

instance (Prefix pfx, BLen a, BLen pfx)
  => BLen (SizePrefixed pfx a) where
    blen ra = blen (lenToPfx @pfx (blen a)) + blen a
      where a = unrefine ra

instance (Prefix pfx, BLen a, Put pfx, Put a)
  => Put (SizePrefixed pfx a) where
    put ra = put (lenToPfx @pfx (blen a)) <> put a
      where a = unrefine ra

class GetSize a where getSize :: Int -> Getter a
instance GetSize B.ByteString where getSize = FP.take

instance (Prefix pfx, GetSize a, Get pfx)
  => Get (CountPrefixed pfx a) where
    get = do
        pfx <- get @pfx
        a <- getSize (pfxToLen pfx)
        pure $ reallyUnsafeRefine a

-- | Types which may be used as prefixes.
--
-- Generally, these will be integer types.
--
-- Note that this is separate to binary representation, so endianness is
-- irrelevant.
--
-- TODO oops can't use 'Int's everywhere because of overflow :'( that's OK
class Prefix a where
    type Max a :: Natural

    -- | used by put. guaranteed that it fits from refined. that is, lenToPfx <=
    --   Max.
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
    lenToPfx _ = error "you lied to refine and broke everything :("
    pfxToLen () = 0

-- I don't think @'Prefix' 'Void'@ is a sensible instance.

-- "I'm not a (w)rapper"
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
