-- | Sized vectors.

{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binrep.Type.Vector where

import Binrep
import Data.Vector.Sized qualified as V
import Data.Vector.Sized ( Vector )
import GHC.TypeNats

type instance CBLen (Vector n a) = CBLen a * n

instance BLen a => BLen (Vector n a) where
    blen = V.sum . V.map blen

instance Put a => Put (Vector n a) where
    put = mconcat . V.toList . V.map put

instance (Get a, KnownNat n) => Get (Vector n a) where
    get = getVector get

getVector :: KnownNat n => Getter a -> Getter (Vector n a)
getVector g = V.replicateM g
