-- | Sized vectors.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binrep.Type.Vector where

import Binrep
import Data.Vector.Sized qualified as VS
import Data.Vector.Sized ( Vector )
import Data.Vector qualified as V
import GHC.TypeNats

import Data.MonoTraversable
import Data.Vector.Generic.Sized.Internal qualified as VS -- for DerivingVia

type instance Element (Vector n a) = a
deriving via V.Vector a instance MonoFunctor     (Vector n a)
deriving via V.Vector a instance MonoFoldable    (Vector n a)
--deriving via V.Vector a instance MonoTraversable (Vector n a)
instance MonoTraversable (Vector n a)

instance BLen a => BLen (Vector n a) where
    blen = blenMonoTraversable

instance Put a => Put (Vector n a) where
    put = putMonoTraversable

instance (Get a, KnownNat n) => Get (Vector n a) where
    get = getVector get

getVector :: KnownNat n => Getter a -> Getter (Vector n a)
getVector g = VS.replicateM g
