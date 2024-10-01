{-# LANGUAGE UndecidableInstances #-} -- required for type-level stuff
{-# LANGUAGE OverloadedStrings #-} -- required for refined errors

module Binrep.Type.Prefix.Size where

import Binrep.Type.Prefix.Internal
import Binrep.Type.Thin
import Binrep
import FlatParse.Basic qualified as FP

import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Data.ByteString qualified as B

import Rerefined.Predicate.Common
import Rerefined.Refine
import TypeLevelShow.Utils

import Data.Kind ( type Type )

data SizePrefix (pfx :: Type)

instance Predicate (SizePrefix pfx) where
    type PredicateName d (SizePrefix pfx) = ShowParen (d > 9)
        ("SizePrefix " ++ LenNatName pfx)

type SizePrefixed pfx = Refined (SizePrefix pfx)

instance
  ( KnownPredicateName (SizePrefix pfx), KnownNat (LenNatMax pfx), BLen a
  ) => Refine (SizePrefix pfx) a where
    validate p a = validateBool p (blen a <= natValInt @(LenNatMax pfx)) $
        "thing too big for length prefix type"

-- TODO no idea if this is sensible
instance IsCBLen (SizePrefixed pfx a) where
    type CBLen (SizePrefixed pfx a) = CBLen pfx + CBLen a

instance (LenNat pfx, BLen a, BLen pfx)
  => BLen (SizePrefixed pfx a) where
    blen ra = blen (lenToNat @pfx (blen a)) + blen a
      where a = unrefine ra

instance (LenNat pfx, BLen a, Put pfx, Put a)
  => Put (SizePrefixed pfx a) where
    put ra = put (lenToNat @pfx (blen a)) <> put a
      where a = unrefine ra

class GetSize a where getSize :: Int -> Getter a
instance GetSize       B.ByteString  where getSize = fmap B.copy . FP.take
instance GetSize (Thin B.ByteString) where getSize = fmap Thin . FP.take

instance (LenNat pfx, GetSize a, Get pfx)
  => Get (SizePrefixed pfx a) where
    get = do
        pfx <- get @pfx
        a <- getSize (natToLen pfx)
        pure $ unsafeRefine a
