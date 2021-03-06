-- | Constant-size data.

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.Sized where

import Binrep
import Binrep.Util ( tshow )

import Refined
import Refined.Unsafe

import GHC.TypeNats
import Data.Typeable ( typeRep )
import FlatParse.Basic qualified as FP

data Size (n :: Natural)

type Sized n a = Refined (Size n) a

type instance CBLen (Sized n a) = n

deriving anyclass instance KnownNat n => BLen (Sized n a)

instance (BLen a, KnownNat n) => Predicate (Size n) a where
    validate p a
     | len > n
        = throwRefineOtherException (typeRep p) $
            "not correctly sized: "<>tshow len<>" /= "<>tshow n
     | otherwise = success
      where
        n = typeNatToBLen @n
        len = blen a

instance Put a => Put (Sized n a) where
    put = put . unrefine

-- TODO safety: isolate consumes all bytes if succeeds
instance (Get a, KnownNat n) => Get (Sized n a) where
    get = do
        a <- FP.isolate (fromIntegral n) get
        return $ reallyUnsafeRefine a
      where
        n = typeNatToBLen @n
