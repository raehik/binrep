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

-- | Essentially runtime reflection of a 'BLen' type to 'CBLen'.
data Size (n :: Natural)
type Sized n = Refined (Size n)

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
deriving via ViaCBLen (Sized n a) instance KnownNat n => BLen (Sized n a)

instance PutC a => PutC (Sized n a) where
    putC = putC . unrefine

instance Put a => Put (Sized n a) where
    put = put . unrefine

instance (Get a, KnownNat n) => Get (Sized n a) where
    get = do
        a <- FP.isolate (natValInt @n) get
        pure $ reallyUnsafeRefine a
        -- ^ REFINE SAFETY: 'FP.isolate' consumes precisely the number of bytes
        -- requested when it succeeds
