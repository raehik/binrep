{-# LANGUAGE UndecidableInstances #-} -- required for type-level stuff
{-# LANGUAGE OverloadedStrings #-} -- required for refined errors

module Binrep.Type.Prefix.Size where

import Binrep.Type.Prefix
import Binrep.Type.Thin
import Binrep
import FlatParse.Basic qualified as FP

import GHC.TypeNats
import Util.TypeNats ( natValInt )
import Data.ByteString qualified as B
import Refined hiding ( Weaken(..), strengthen )
import Refined.Unsafe

import Data.Typeable ( Typeable, typeRep )
import Data.Kind

data SizePrefix (pfx :: Type)
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
instance GetSize       B.ByteString  where getSize = fmap B.copy . FP.take
instance GetSize (Thin B.ByteString) where getSize = fmap Thin . FP.take

instance (Prefix pfx, GetSize a, Get pfx)
  => Get (SizePrefixed pfx a) where
    get = do
        pfx <- get @pfx
        a <- getSize (pfxToLen pfx)
        pure $ reallyUnsafeRefine a
