-- | Constant-size data.

{-# LANGUAGE UndecidableInstances #-} -- for PredicateName
{-# LANGUAGE OverloadedStrings #-} -- for refine error builder

module Binrep.Type.Sized where

import Binrep
import FlatParse.Basic qualified as FP

import Rerefined.Predicate.Common
import Rerefined.Refine
import TypeLevelShow.Natural
import TypeLevelShow.Utils
import Data.Text.Builder.Linear qualified as TBL

import GHC.TypeNats
import Util.TypeNats ( natValInt )

-- | Essentially runtime reflection of a 'BLen' type to 'CBLen'.
data Size (n :: Natural)

instance Predicate (Size n) where
    type PredicateName d (Size n) = ShowParen (d > 9)
        ("Size " ++ ShowNatDec n)

type Sized n = Refined (Size n)

instance (KnownPredicateName (Size n), BLen a, KnownNat n)
  => Refine (Size n) a where
    validate p a = validateBool p e (len == n)
      where
        n = natValInt @n
        len = blen a
        e = "not correctly sized: "<>TBL.fromDec len<>" /= "<>TBL.fromDec n

instance IsCBLen (Sized n a) where type CBLen (Sized n a) = n
deriving via ViaCBLen (Sized n a) instance KnownNat n => BLen (Sized n a)

instance PutC a => PutC (Sized n a) where
    putC = putC . unrefine

instance Put a => Put (Sized n a) where
    put = put . unrefine

instance (Get a, KnownNat n) => Get (Sized n a) where
    get = do
        a <- FP.isolate (natValInt @n) get
        pure $ unsafeRefine a
        -- ^ REFINE SAFETY: 'FP.isolate' consumes precisely the number of bytes
        -- requested when it succeeds
