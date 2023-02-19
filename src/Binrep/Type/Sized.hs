-- | Constant-size data.

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.Sized where

import Binrep
import FlatParse.Basic qualified as FP

import Binrep.Util ( tshow )

import Refined
import Refined.Unsafe
import Data.Typeable ( typeRep )

import GHC.TypeNats
import Util.TypeNats ( natValInt )

-- | Essentially reflects a 'BLen' type to 'CBLen'.
data Size (n :: Natural)
type Sized n a = Refined (Size n) a

instance (BLen a, KnownNat n) => Predicate (Size n) a where
    validate p a
     | len /= n
        = throwRefineOtherException (typeRep p) $
            "not correctly sized: "<>tshow len<>" /= "<>tshow n
     | otherwise = success
      where
        n = natValInt @n
        len = blen a

instance IsCBLen (Sized n a) where type CBLen (Sized n a) = n
deriving via CBLenly (Sized n a) instance KnownNat n => BLen (Sized n a)

instance Put a => Put (Sized n a) where
    put = put . unrefine

-- TODO safety: isolate consumes all bytes if succeeds
instance (Get a, KnownNat n) => Get (Sized n a) where
    get = do
        a <- FP.isolate (natValInt @n) get
        pure $ reallyUnsafeRefine a
